{
module Language.TinyErlang.Lexer (lex) where

import TEPrelude
import Language.TinyErlang.Tokens (Token (..))
import qualified Data.Text as Text
}

%wrapper "basic"

$digit = [0-9]
$Alpha = [A-Z]
$alpha = [a-zA-Z]

tokens :-
  $white+                     ;
  "%".*                       ;
  "(" | ")" | "}" | "{" | "[" | "]" |
  "|" | "," | "." | ";" | "=" | "_" |
  "->"                        { TokSymbol . Text.pack }
  \" [^ \"]* \"               { \s -> TokStringLit ((Text.tail . Text.init . Text.pack) s) }
  $digit+				              { \s -> TokIntegerLit (read s) }
  $Alpha [$alpha $digit \_]*  { \s -> TokVar (Text.pack s) }
  $alpha [$alpha $digit \_]*  { \s -> TokAtom (Text.pack s) }
  \' [^ \']* \'               { \s -> TokAtom ((Text.tail . Text.init . Text.pack) s) }

{
lex :: Text -> [Token]
lex = alexScanTokens . Text.unpack
}