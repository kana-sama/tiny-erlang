module Language.TinyErlang.Tokens (Token (..)) where

import TEPrelude

data Token
  = TokAtom Text
  | TokVar Text
  | TokStringLit Text
  | TokIntegerLit Integer
  | TokSymbol Text
  deriving stock (Eq, Ord, Show)
