module Tiny.Ast (BinOp(..), Expr(..), Stmt(..)) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

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

derive instance Eq BinOp

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
  | NullLit
  | Var String
  | BinExpr Expr BinOp Expr

derive instance Eq Expr

instance Show Expr where
  show (IntLit value) = show value
  show (BoolLit value) = show value
  show NullLit = "null"
  show (Var name) = name
  show (BinExpr lhs op rhs) =
    "("
      <> show lhs
      <> " "
      <> show op
      <> " "
      <> show rhs
      <> ")"

data Stmt
  = VarStmt String Expr
  | IfStmt Expr (Array Stmt) (Maybe (Array Stmt))

derive instance Eq Stmt

showBlock :: Array Stmt -> String
showBlock stmts = "{ " <> joinWith " " (show <$> stmts) <> " }"

instance Show Stmt where
  show (VarStmt name value) =
    "var "
      <> name
      <> " = "
      <> show value
      <> ";"
  show (IfStmt cond thenBody maybeElseBody) =
    "if "
      <> show cond
      <> " "
      <> showBlock thenBody
      <> case maybeElseBody of
        Just elseBody -> " else " <> showBlock elseBody
        Nothing -> ""
