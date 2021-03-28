{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

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
  | LitAtom Atom
  | LitVar Var
  deriving stock (Show, Functor, Foldable, Traversable)

data Expr
  = ExprLit (Lit Expr)
  | ExprCall Atom [Expr]
  | ExprMatch Pat Expr
  | ExprCase Expr [(Pat, [Expr])]
  | ExprRecieve [(Pat, [Expr])] (Maybe (Expr, [Expr]))
  | ExprSend Expr Expr
  deriving stock (Show)

data Pat
  = PatLit (Lit Pat)
  | PatHole
  | PatMatch Pat Pat
  deriving stock (Show)

data FunClause
  = FunClause [Pat] [Expr]
  deriving stock (Show)

data FunDecl
  = FunDecl Atom [FunClause]
  deriving stock (Show)
