module Language.TinyErlang.Parser where

import Language.TinyErlang.AST
import Language.TinyErlang.Lexer (lex)
import Language.TinyErlang.Tokens as TE (Token (..))
import TEPrelude hiding (many)
import Text.Megaparsec

newtype Parser a = Parser {unParser :: Parsec Void [TE.Token] a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadPlus, Alternative)
  deriving newtype (MonadParsec Void [TE.Token])

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

tok :: String -> Parser ()
tok "(" = void $ single TokOpenParen
tok ")" = void $ single TokCloseParen
tok "{" = void $ single TokOpenBrace
tok "}" = void $ single TokCloseBrace
tok "[" = void $ single TokOpenBracket
tok "]" = void $ single TokCloseBracket
tok "|" = void $ single TokVLine
tok "," = void $ single TokComma
tok "." = void $ single TokDot
tok ";" = void $ single TokSemicolon
tok "=" = void $ single TokEquals
tok "_" = void $ single TokHole
tok "->" = void $ single TokArrow
tok _ = error "unknown tok"

-- parsers

parens, braces, brackets :: Parser a -> Parser a
parens = between (tok "(") (tok ")")
braces = between (tok "{") (tok "}")
brackets = between (tok "[") (tok "]")

atom :: Parser Atom
atom = Atom <$> tokAtom

var :: Parser Var
var = Var <$> tokVar

list :: Parser expr -> (Lit expr -> expr) -> Parser expr
list expr inj = brackets (try tail <|> try inline)
  where
    toLitList rest =
      foldr (\a b -> inj (LitListCons a b)) rest

    inline = do
      xs <- expr `sepBy` tok ","
      pure (toLitList (inj LitListEmpty) xs)

    tail = do
      xs <- expr `sepBy1` tok ","
      tok "|"
      rest <- expr
      pure (toLitList rest xs)

expr :: Parser Expr
expr =
  choice
    [ try $ ExprCall <$> atom <*> parens (expr `sepBy` tok ","),
      try $ ExprAssign <$> pat <*> (tok "=" *> expr),
      ExprAtom <$> atom,
      ExprVar <$> var,
      ExprLit . LitInteger <$> tokInt,
      ExprLit . LitString <$> tokStr,
      ExprLit . LitTuple <$> braces (expr `sepBy` tok ","),
      list expr ExprLit,
      parens expr
    ]
  where
    call = ExprCall <$> atom <*> parens (expr `sepBy` tok ",")

pat :: Parser Pat
pat =
  choice
    [ PatAtom <$> atom,
      PatVar <$> var,
      PatLit . LitInteger <$> tokInt,
      PatLit . LitString <$> tokStr,
      PatLit . LitTuple <$> braces (pat `sepBy` tok ","),
      list pat PatLit,
      PatHole <$ tok "_"
    ]

clause :: Parser (Atom, FunClause)
clause = do
  name <- atom
  args <- parens (pat `sepBy` tok ",")
  tok "->"
  values <- expr `sepBy` tok ","
  pure (name, FunClause args values)

fun :: Parser FunDecl
fun = do
  clauses' <- clause `sepBy1` tok ";" <* tok "."
  let (names, clauses) = unzip clauses'
  let name = head names
  guard (all (== name) names)
  pure (FunDecl name clauses)

decls :: Parser [FunDecl]
decls = many fun <* eof

-- entry point

runP :: Parser a -> [TE.Token] -> a
runP p toks =
  case runParser (unParser p) "_.erl" toks of
    Right val -> val
    Left err -> error (show err)

parse :: [TE.Token] -> [FunDecl]
parse = runP decls
