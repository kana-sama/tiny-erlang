module Language.TinyErlang.AST where

import qualified Data.Text as Text
import TEPrelude

newtype Atom = Atom Text
  deriving newtype (IsString, Eq, Ord)
  deriving stock (Show)

newtype Var = Var Text
  deriving newtype (IsString, Eq, Ord)
  deriving stock (Show)

data Lit expr
  = LitInteger Integer
  | LitString Text
  | LitTuple [expr]
  | LitListEmpty
  | LitListCons expr expr
  deriving stock (Show)

data Expr
  = ExprAtom Atom
  | ExprVar Var
  | ExprLit (Lit Expr)
  | ExprCall Atom [Expr]
  | ExprAssign Pat Expr
  deriving stock (Show)

data Pat
  = PatAtom Atom
  | PatVar Var
  | PatLit (Lit Pat)
  | PatHole
  deriving stock (Show)

data FunClause
  = FunClause [Pat] [Expr]
  deriving stock (Show)

data FunDecl
  = FunDecl Atom [FunClause]
  deriving stock (Show)
