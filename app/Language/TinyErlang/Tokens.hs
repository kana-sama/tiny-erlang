module Language.TinyErlang.Tokens (Token (..)) where

import TEPrelude

data Token
  = TokAtom Text
  | TokVar Text
  | TokStringLit Text
  | TokIntegerLit Integer
  | TokSymbol Text
  | TokCase
  | TokRecieve
  | TokAfter
  | TokOf
  | TokEnd
  deriving stock (Eq, Ord, Show)
