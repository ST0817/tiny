module Tiny.Ast (BinOp(..), Expr(..)) where

import Prelude

data BinOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | ModOp
  | PowOp
  | EqOp
  | NotEqOp
  | GTOp
  | LTOp
  | GEOp
  | LEOp
  | AndOp
  | OrOp

instance Show BinOp where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"
  show DivOp = "/"
  show ModOp = "%"
  show PowOp = "**"
  show EqOp = "=="
  show NotEqOp = "!="
  show GTOp = ">"
  show LTOp = "<"
  show GEOp = ">="
  show LEOp = "<="
  show AndOp = "&&"
  show OrOp = "||"

data Expr
  = IntLit Int
  | BoolLit Boolean
  | BinExpr Expr BinOp Expr

instance Show Expr where
  show (IntLit value) = show value
  show (BoolLit value) = show value
  show (BinExpr lhs op rhs) = "("
    <> show lhs
    <> " "
    <> show op
    <> " "
    <> show rhs
    <> ")"
