module Test.Tiny.Parsing.Stmt (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..), Pattern(..), Stmt(..))
import Tiny.Parsing (parseStmts)

spec :: Spec Unit
spec = describe "statement" do
  it "variable statement" do
    shouldEqual
      (runParser "var foo = 42;" parseStmts)
      (Right [ VarStmt (VarPattern "foo") (IntLit 42) ])

  it "variable statement (default value)" do
    shouldEqual
      (runParser "var foo;" parseStmts)
      (Right [ VarStmt (VarPattern "foo") NullLit ])

  it "assignment statement" do
    shouldEqual
      (runParser "foo = 42;" parseStmts)
      (Right [ AssignStmt (VarPattern "foo") (IntLit 42) ])

  it "if statement (then only)" do
    let
      input =
        """
          if foo > 10 {
              var bar = 1;
          }
          """
      cond = BinExpr (Var "foo") GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
    shouldEqual
      (runParser input $ parseStmts)
      (Right [ IfStmt cond thenBody Nothing ])

  it "if statement (then and else)" do
    let
      input =
        """
          if foo > 10 {
              var bar = 1;
          } else {
              var bar = 2;
          }
          """
      cond = BinExpr (Var "foo") GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
      elseBody = [ VarStmt (VarPattern "bar") (IntLit 2) ]
    shouldEqual
      (runParser input parseStmts)
      (Right [ IfStmt cond thenBody $ Just elseBody ])

  it "if statement (else if)" do
    let
      input =
        """
          if foo > 10 {
              var bar = 1;
          } else if foo > 5 {
              var bar = 2;
          } else {
              var bar = 3;
          }
          """
      cond = BinExpr (Var "foo") GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
      elseIfCond = BinExpr (Var "foo") GTOp (IntLit 5)
      elseIfThenBody = [ VarStmt (VarPattern "bar") (IntLit 2) ]
      elseIfElseBody = [ VarStmt (VarPattern "bar") (IntLit 3) ]
      elseBody = [ IfStmt elseIfCond elseIfThenBody $ Just elseIfElseBody ]
    shouldEqual
      (runParser input parseStmts)
      (Right [ IfStmt cond thenBody $ Just elseBody ])
