module Language.TinyErlang.Parser where

import qualified Data.Text as Text
import Language.TinyErlang.AST
import Language.TinyErlang.CST
import Language.TinyErlang.Lexer (lex)
import Language.TinyErlang.Tokens (Token (..))
import TEPrelude hiding (many)
import Text.Megaparsec hiding (Token)

newtype Parser a = Parser {unParser :: Parsec Void [Token] a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)
  deriving newtype (MonadPlus, Alternative)
  deriving newtype (MonadParsec Void [Token])

instance a ~ () => IsString (Parser a) where
  fromString = tok . Text.pack

-- tokens

tokAtom :: Parser Text
tokAtom = flip token mempty \case
  TokAtom a -> pure a
  _ -> Nothing

tokVar :: Parser Text
tokVar = flip token mempty \case
  TokVar a -> pure a
  _ -> Nothing

tokStr :: Parser Text
tokStr = flip token mempty \case
  TokStringLit a -> pure a
  _ -> Nothing

tokInt :: Parser Integer
tokInt = flip token mempty \case
  TokIntegerLit a -> pure a
  _ -> Nothing

tokCase, tokRecieve, tokAfter, tokOf, tokEnd :: Parser ()
tokCase = void (single TokCase)
tokRecieve = void (single TokRecieve)
tokAfter = void (single TokAfter)
tokOf = void (single TokOf)
tokEnd = void (single TokEnd)

tok :: Text -> Parser ()
tok s = void $ single (TokSymbol s)

-- parsers

parens, braces, brackets :: Parser a -> Parser a
parens = between "(" ")"
braces = between "{" "}"
brackets = between "[" "]"

atom :: Parser Atom
atom = Atom <$> tokAtom

var :: Parser Var
var = Var <$> tokVar

list :: Parser expr -> (Lit expr -> expr) -> Parser expr
list expr inj = brackets (try tail <|> inline)
  where
    toLitList rest =
      foldr (\a b -> inj (LitListCons a b)) rest

    tail = do
      xs <- expr `sepBy1` ","
      rest <- "|" *> expr
      pure (toLitList rest xs)

    inline = do
      xs <- expr `sepBy` ","
      pure (toLitList (inj LitListEmpty) xs)

caseof :: Parser CExpr
caseof = do
  val <- tokCase *> cexpr
  tokOf
  branches <- branch `sepBy` ";"
  tokEnd
  pure (CExprCase val branches)
  where
    branch = (,) <$> cexpr <* "->" <*> (cexpr `sepBy1` ",")

receive :: Parser CExpr
receive = between tokRecieve tokEnd (try withAfter <|> withoutAfter)
  where
    withAfter = do
      branches <- branch `sepBy` ";"
      tokAfter
      after <- branch
      pure (CExprRecieve branches (Just after))
    withoutAfter = do
      branches <- branch `sepBy1` ";"
      pure (CExprRecieve branches Nothing)
    branch = (,) <$> cexpr <* "->" <*> (cexpr `sepBy1` ",")

cexpr, cexpr1 :: Parser CExpr
cexpr =
  choice
    [ try (CExprMatch <$> cexpr1 <* "=" <*> cexpr),
      try (CExprSend <$> cexpr1 <* "!" <*> cexpr),
      cexpr1
    ]
cexpr1 =
  choice
    [ try (CExprCall <$> atom <* "(" <*> (cexpr `sepBy` ",") <* ")"),
      CExprLit . LitAtom <$> atom,
      CExprLit . LitVar <$> var,
      CExprLit . LitInteger <$> tokInt,
      CExprLit . LitString <$> tokStr,
      CExprLit . LitTuple <$> braces (cexpr `sepBy` ","),
      list cexpr CExprLit,
      caseof,
      receive,
      parens cexpr
    ]

expr :: Parser Expr
expr = do Just x <- convertToExpr <$> cexpr; pure x

pat :: Parser Pat
pat = do Just x <- convertToPat <$> cexpr; pure x

clause :: Parser (Atom, FunClause)
clause = do
  name <- atom
  args <- parens (pat `sepBy` ",")
  values <- "->" *> expr `sepBy` ","
  pure (name, FunClause args values)

fun :: Parser FunDecl
fun = do
  clauses' <- (clause `sepBy1` ";") <* "."
  let (names, clauses) = unzip clauses'
  let name = head names
  guard (all (== name) names)
  pure (FunDecl name clauses)

decls :: Parser [FunDecl]
decls = many fun

-- entry point

runP :: Parser a -> [Token] -> a
runP p toks =
  case runParser (unParser p <* eof) "_.erl" toks of
    Right val -> val
    Left err -> error (show err)

parse :: [Token] -> [FunDecl]
parse = runP decls
