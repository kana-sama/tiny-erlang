module Language.TinyErlang.Tokens (Token (..)) where

import TEPrelude

data Token
  = TokAtom Text
  | TokVar Text
  | TokStringLit Text
  | TokIntegerLit Integer
  | TokOpenParen
  | TokCloseParen
  | TokOpenBrace
  | TokCloseBrace
  | TokOpenBracket
  | TokCloseBracket
  | TokVLine
  | TokComma
  | TokDot
  | TokSemicolon
  | TokArrow
  | TokEquals
  | TokHole
  deriving stock (Eq, Ord, Show)
