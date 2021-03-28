{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Language.TinyErlang.Interpreter (eval) where

import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.TinyErlang.AST
import TEPrelude

newtype Pid = Pid Int
  deriving (Eq, Ord)

type SyncIO = SyncIOCmd -> IO ()

data SyncIOCmd = Stop | SyncCommand (IO ())

withSyncIO :: ((?syncIO :: SyncIO) => IO ()) -> IO ()
withSyncIO next = do
  commands <- newTQueueIO
  pid <- async (go commands)
  let ?syncIO = atomically . writeTQueue commands in next
  atomically (writeTQueue commands Stop)
  wait pid
  where
    go commands = do
      atomically (readTQueue commands) >>= \case
        Stop -> pure ()
        SyncCommand cmd -> cmd >> go commands

sync :: (?syncIO :: SyncIO) => IO () -> IO ()
sync action = ?syncIO (SyncCommand action)

type RuntimeContext =
  ( ?functions :: TVar (Map (Atom, Int) Fun),
    ?processes :: TVar (Map Pid (TQueue Val, Async ())),
    ?gen :: TVar Int,
    ?syncIO :: SyncIO
  )

type ProcessContext =
  (?self :: Pid)

type FunctionContext =
  (?scope :: TVar (Map Var Val))

data Val
  = ValInt Integer
  | ValString Text
  | ValAtom Atom
  | ValTuple [Val]
  | ValList [Val]
  | ValPid Pid
  deriving (Eq)

displayValue :: Val -> Text
displayValue (ValInt i) = showT i
displayValue (ValString s) = s
displayValue (ValAtom (Atom a)) = a
displayValue (ValTuple vals) = "{" <> Text.intercalate "," (displayValue <$> vals) <> "}"
displayValue (ValList vals) = "[" <> Text.intercalate "," (displayValue <$> vals) <> "]"
displayValue (ValPid (Pid pid)) = "<" <> showT pid <> ">"

newtype Fun = Fun {runFun :: (RuntimeContext, ProcessContext) => [Val] -> IO Val}

throw :: Text -> IO a
throw = errorT

lookupVarSafe :: FunctionContext => Var -> IO (Maybe Val)
lookupVarSafe var = atomically do
  scope <- readTVar ?scope
  pure (Map.lookup var scope)

lookupVar :: FunctionContext => Var -> IO Val
lookupVar var@(Var name) =
  lookupVarSafe var >>= \case
    Nothing -> throw ("unknown var: " <> name)
    Just val -> pure val

insertVar :: FunctionContext => Var -> Val -> IO ()
insertVar var val = atomically do
  modifyTVar' ?scope (Map.insert var val)

lookupFun :: RuntimeContext => (Atom, Int) -> IO Fun
lookupFun sign@(Atom name, arity) = do
  functions <- readTVarIO ?functions
  case Map.lookup sign functions of
    Nothing -> throw ("unknown function: " <> name <> "/" <> showT arity)
    Just fun -> pure fun

declareFun :: RuntimeContext => (Atom, Int) -> Fun -> IO ()
declareFun sign fun = atomically do
  modifyTVar' ?functions (Map.insert sign fun)

spawnProcess :: RuntimeContext => Atom -> [Val] -> IO Pid
spawnProcess funName args = do
  fun <- lookupFun (funName, length args)
  (pid, mailbox) <- atomically do
    pid <- stateTVar ?gen \gen -> (Pid gen, gen + 1)
    mailbox <- newTQueue
    pure (pid, mailbox)
  handle <- let ?self = pid in async (runFun fun args)
  atomically do modifyTVar' ?processes (Map.insert pid (mailbox, void handle))
  pure pid

waitForProcess :: RuntimeContext => Pid -> IO ()
waitForProcess pid = do
  processes <- readTVarIO ?processes
  case Map.lookup pid processes of
    Nothing -> pure ()
    Just (_, handle) -> wait handle

matchValSafe :: Val -> Val -> ExceptT Text IO ()
matchValSafe val val' | val == val' = pure ()
matchValSafe val val' = throwError ("Impossible to match value " <> displayValue val <> " with value " <> displayValue val')

matchSafe :: FunctionContext => Pat -> Val -> ExceptT Text IO ()
matchSafe (PatLit (LitAtom atomP)) (ValAtom atomV) | atomP == atomV = pure ()
matchSafe (PatLit (LitVar var)) val = do
  lift (lookupVarSafe var) >>= \case
    Nothing -> lift (insertVar var val)
    Just val' -> matchValSafe val val'
matchSafe (PatLit (LitInteger i)) v = matchValSafe (ValInt i) v
matchSafe (PatLit (LitString s)) v = matchValSafe (ValString s) v
matchSafe (PatLit (LitTuple pats)) (ValTuple vals) = matchManySafe pats vals
matchSafe (PatLit LitListEmpty) (ValList []) = pure ()
matchSafe (PatLit (LitListCons h t)) (ValList (x : xs)) = matchSafe h x >> matchSafe t (ValList xs)
matchSafe (PatMatch a b) val = matchSafe a val *> matchSafe b val
matchSafe PatHole _ = pure ()
matchSafe pat val = throwError ("Impossible to match value " <> displayValue val <> " with pattern " <> showT pat)

matchManySafe :: FunctionContext => [Pat] -> [Val] -> ExceptT Text IO ()
matchManySafe pats vals | length pats /= length vals = throwError "invalid lengths"
matchManySafe pats vals = traverse_ (uncurry matchSafe) (zip pats vals)

match :: FunctionContext => Pat -> Val -> IO ()
match pat val =
  runExceptT (matchSafe pat val) >>= \case
    Right () -> pure ()
    Left err -> throw err

getMailboxSTM :: RuntimeContext => Pid -> STM (TQueue Val)
getMailboxSTM pid = do
  processes <- readTVar ?processes
  case Map.lookup pid processes of
    Just (mailbox, _) -> pure mailbox
    Nothing -> errorT ("no mailbox for " <> displayValue (ValPid pid))

evalExpr :: (ProcessContext, RuntimeContext, FunctionContext) => Expr -> IO Val
evalExpr (ExprLit (LitAtom atom)) = pure (ValAtom atom)
evalExpr (ExprLit (LitVar var)) = lookupVar var
evalExpr (ExprLit (LitInteger i)) = pure (ValInt i)
evalExpr (ExprLit (LitString s)) = pure (ValString s)
evalExpr (ExprLit (LitTuple xs)) = ValTuple <$> traverse evalExpr xs
evalExpr (ExprLit LitListEmpty) = pure (ValList [])
evalExpr (ExprLit (LitListCons h t)) = do
  h' <- evalExpr h
  t' <- evalExpr t
  case t' of
    ValList t'' -> pure (ValList (h' : t''))
    _ -> throw ("Used as list: " <> displayValue t')
evalExpr (ExprCall f args) = do
  Fun fun <- lookupFun (f, length args)
  fun =<< traverse evalExpr args
evalExpr (ExprMatch pat expr) = do
  val <- evalExpr expr
  match pat val
  pure val
evalExpr (ExprCase val branches) = do
  val' <- evalExpr val
  go branches val'
  where
    go [] arg = throw ("No clause for " <> displayValue arg)
    go ((pat, next) : branches) arg = do
      runExceptT (matchSafe pat arg) >>= \case
        Right () -> last <$> traverse evalExpr next
        Left err -> go branches arg
evalExpr (ExprRecieve branches after) = do
  withAfter after do
    msg <- atomically do readTQueue =<< getMailboxSTM ?self
    go branches msg
  where
    go [] arg = throw ("No clause for " <> displayValue arg)
    go ((pat, next) : branches) arg = do
      runExceptT (matchSafe pat arg) >>= \case
        Right () -> last <$> traverse evalExpr next
        Left err -> go branches arg

    withAfter Nothing next = next
    withAfter (Just (timeout', after)) next = do
      ValInt timeout <- evalExpr timeout'
      either id id <$> race next do
        threadDelay (fromInteger timeout * 1000)
        last <$> traverse evalExpr after
evalExpr (ExprSend pid' val) = do
  ValPid pid <- evalExpr pid'
  msg <- evalExpr val
  atomically do
    mailbox <- getMailboxSTM pid
    writeTQueue mailbox msg
  pure msg

compileFun :: RuntimeContext => FunDecl -> Fun
compileFun (FunDecl (Atom name) clauses) = Fun \args -> do
  scope <- newTVarIO Map.empty
  let ?scope = scope in go args clauses
  where
    go :: (FunctionContext, ProcessContext) => [Val] -> [FunClause] -> IO Val
    go args [] = throw ("No clause for " <> name <> "(" <> Text.intercalate ", " (displayValue <$> args) <> ")")
    go args (FunClause pats values : clauses) = do
      atomically do writeTVar ?scope Map.empty
      result <-
        runExceptT (matchManySafe pats args) >>= \case
          Right () -> last <$> traverse evalExpr values
          Left err -> go args clauses
      pure result

evalModule :: RuntimeContext => [FunDecl] -> IO ()
evalModule decls = do
  for_ decls \decl@(FunDecl name clauses) -> do
    let arity = let FunClause pats _ = head clauses in length pats
    declareFun (name, arity) (compileFun decl)
  waitForProcess =<< spawnProcess "start" []

eval :: [FunDecl] -> IO ()
eval decls = withSyncIO do
  processes <- newTVarIO Map.empty
  gen <- newTVarIO 0
  functions <- newTVarIO bifs
  let ?processes = processes
      ?gen = gen
      ?functions = functions
   in evalModule decls
  where
    bifs =
      Map.fromList
        [ (("plus", 2), plus_2),
          (("print", 0), print_n),
          (("print", 1), print_n),
          (("print", 2), print_n),
          (("print", 3), print_n),
          (("print", 4), print_n),
          (("self", 0), self_0),
          (("spawn", 3), spawn_3)
        ]

    print_n = Fun \args -> do
      sync $ liftIO . putStrLn . mconcat . fmap displayValue $ args
      pure (ValAtom (Atom "ok"))

    plus_2 = Fun \case
      [ValInt a, ValInt b] -> pure (ValInt (a + b))
      _ -> throw "only numbers are accepted for plus"

    self_0 = Fun \_ -> pure (ValPid ?self)

    spawn_3 = Fun \case
      [ValAtom mod, ValAtom fun, ValList args] -> ValPid <$> spawnProcess fun args
      _ -> throw "invalid args for spawn/3(atom, atom, value)"
