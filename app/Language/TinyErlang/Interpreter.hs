module Language.TinyErlang.Interpreter (eval) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.TinyErlang.AST
import TEPrelude

newtype Runtime a = Runtime {unRuntime :: StateT Env IO a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState Env, MonadIO)

data Env = Env
  { functions :: Map (Atom, Int) Fun,
    scope :: Map Var Val
  }

data Val
  = ValInt Integer
  | ValString Text
  | ValAtom Atom
  | ValTuple [Val]
  | ValList [Val]
  deriving (Eq)

displayValue :: Val -> Text
displayValue (ValInt i) = showT i
displayValue (ValString s) = s
displayValue (ValAtom (Atom a)) = a
displayValue (ValTuple vals) = "{" <> Text.intercalate "," (displayValue <$> vals) <> "}"
displayValue (ValList vals) = "[" <> Text.intercalate "," (displayValue <$> vals) <> "]"

newtype Fun = Fun {runFun :: [Val] -> Runtime Val}

throw :: Text -> Runtime a
throw = errorT

withEmptyScope :: Runtime a -> Runtime a
withEmptyScope next = do
  env <- get
  put env {scope = Map.empty}
  result <- next
  put env
  pure result

lookupVarSafe :: Var -> Runtime (Maybe Val)
lookupVarSafe var = do
  Env {scope} <- get
  pure (Map.lookup var scope)

lookupVar :: Var -> Runtime Val
lookupVar var@(Var name) =
  lookupVarSafe var >>= \case
    Nothing -> throw ("unknown var: " <> name)
    Just val -> pure val

insertVar :: Var -> Val -> Runtime ()
insertVar var val = do
  modify \env@Env {scope} -> env {scope = Map.insert var val scope}

lookupFun :: (Atom, Int) -> Runtime Fun
lookupFun sign@(Atom name, arity) = do
  Env {functions} <- get
  case Map.lookup sign functions of
    Nothing -> throw ("unknown function: " <> name <> "/" <> showT arity)
    Just fun -> pure fun

declareFun :: (Atom, Int) -> Fun -> Runtime ()
declareFun sign fun = modify \env@Env {functions} ->
  env {functions = Map.insert sign fun functions}

matchValSafe :: Val -> Val -> ExceptT Text Runtime ()
matchValSafe val val' | val == val' = pure ()
matchValSafe val val' = throwError ("Impossible to match value " <> displayValue val <> " with value " <> displayValue val')

matchSafe :: Pat -> Val -> ExceptT Text Runtime ()
matchSafe (PatAtom atomP) (ValAtom atomV) | atomP == atomV = pure ()
matchSafe (PatVar var) val = do
  lift (lookupVarSafe var) >>= \case
    Nothing -> lift (insertVar var val)
    Just val' -> matchValSafe val val'
matchSafe (PatLit (LitInteger i)) v = matchValSafe (ValInt i) v
matchSafe (PatLit (LitString s)) v = matchValSafe (ValString s) v
matchSafe (PatLit (LitTuple pats)) (ValTuple vals) = matchManySafe pats vals
matchSafe (PatLit LitListEmpty) (ValList []) = pure ()
matchSafe (PatLit (LitListCons h t)) (ValList (x : xs)) = matchSafe h x >> matchSafe t (ValList xs)
matchSafe PatHole _ = pure ()
matchSafe pat val = throwError ("Impossible to match value " <> displayValue val <> " with pattern " <> showT pat)

matchManySafe :: [Pat] -> [Val] -> ExceptT Text Runtime ()
matchManySafe pats vals | length pats /= length vals = throwError "invalid lengths"
matchManySafe pats vals = traverse_ (uncurry matchSafe) (zip pats vals)

match :: Pat -> Val -> Runtime ()
match pat val =
  runExceptT (matchSafe pat val) >>= \case
    Right () -> pure ()
    Left err -> throw err

evalExpr :: Expr -> Runtime Val
evalExpr (ExprAtom atom) = pure (ValAtom atom)
evalExpr (ExprVar var) = lookupVar var
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
evalExpr (ExprAssign pat expr) = do
  val <- evalExpr expr
  match pat val
  pure val

compileFun :: FunDecl -> Fun
compileFun (FunDecl (Atom name) clauses) = Fun \args -> go args clauses
  where
    go args [] = throw ("No clause for " <> name <> "(" <> Text.intercalate ", " (displayValue <$> args) <> ")")
    go args (FunClause pats values : clauses) = do
      withEmptyScope do
        runExceptT (matchManySafe pats args) >>= \case
          Right () -> last <$> traverse evalExpr values
          Left err -> go args clauses

evalModule :: [FunDecl] -> Runtime ()
evalModule decls = do
  for_ decls \decl@(FunDecl name clauses) -> do
    let arity = let FunClause pats _ = head clauses in length pats
    declareFun (name, arity) (compileFun decl)
  main <- lookupFun ("main", 0)
  runFun main []
  pure ()

eval :: [FunDecl] -> IO ()
eval decls = evalStateT (unRuntime (evalModule decls)) defaultEnv
  where
    defaultEnv =
      Env
        { functions =
            Map.fromList
              [ (("plus", 2), plus_2),
                (("print", 0), print_n),
                (("print", 1), print_n),
                (("print", 2), print_n),
                (("print", 3), print_n),
                (("print", 4), print_n)
              ],
          scope = Map.empty
        }

    print_n = Fun \args -> do
      liftIO . putStrLn . mconcat . fmap displayValue $ args
      pure (ValAtom (Atom "ok"))

    plus_2 = Fun \case
      [ValInt a, ValInt b] -> pure (ValInt (a + b))
      _ -> throw "only numbers are accepted for plus"
