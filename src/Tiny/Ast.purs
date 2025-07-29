module Tiny.Ast (BinOp(..), Expr(..)) where

import Prelude

data BinOp = AddOp | SubOp

instance Show BinOp where
  show AddOp = "+"
  show SubOp = "-"

data Expr
  = IntLit Int
  | BinExpr Expr BinOp Expr

instance Show Expr where
  show (IntLit value) = show value
  show (BinExpr lhs op rhs) = "("
    <> show lhs
    <> " "
    <> show op
    <> " "
    <> show rhs
    <> ")"
