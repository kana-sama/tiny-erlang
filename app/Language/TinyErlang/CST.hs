module Language.TinyErlang.CST where

import Language.TinyErlang.AST
import TEPrelude

data CExpr
  = CExprLit (Lit CExpr)
  | CExprCall Atom [CExpr]
  | CExprMatch CExpr CExpr
  | CExprCase CExpr [(CExpr, [CExpr])]
  | CExprRecieve [(CExpr, [CExpr])] (Maybe (CExpr, [CExpr]))
  | CExprSend CExpr CExpr
  deriving stock (Show)

convertBranch :: (CExpr, [CExpr]) -> Maybe (Pat, [Expr])
convertBranch (p, e) = (,) <$> convertToPat p <*> traverse convertToExpr e

convertToExpr :: CExpr -> Maybe Expr
convertToExpr (CExprLit l) = ExprLit <$> traverse convertToExpr l
convertToExpr (CExprCall f args) = ExprCall f <$> traverse convertToExpr args
convertToExpr (CExprMatch pat val) = ExprMatch <$> convertToPat pat <*> convertToExpr val
convertToExpr (CExprCase val branches) = ExprCase <$> convertToExpr val <*> traverse convertBranch branches
convertToExpr (CExprRecieve branches after) =
  ExprRecieve
    <$> traverse convertBranch branches
    <*> traverse (\(p, e) -> (,) <$> convertToExpr p <*> traverse convertToExpr e) after
convertToExpr (CExprSend a b) = ExprSend <$> convertToExpr a <*> convertToExpr b

convertToPat :: CExpr -> Maybe Pat
convertToPat (CExprLit (LitVar "_")) = pure PatHole
convertToPat (CExprLit l) = PatLit <$> traverse convertToPat l
convertToPat (CExprCall _ _) = Nothing
convertToPat (CExprMatch a b) = PatMatch <$> convertToPat a <*> convertToPat b
convertToPat (CExprCase _ _) = Nothing
convertToPat (CExprRecieve _ _) = Nothing
convertToPat (CExprSend _ _) = Nothing
