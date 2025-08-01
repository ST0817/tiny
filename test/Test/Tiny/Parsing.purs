module Test.Tiny.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))
import Tiny.Parsing (parseSingleExpr, parseStmts)

spec :: Spec Unit
spec = describe "parsing" do
  describe "expression" do
    it "integer literal" do
      shouldEqual
        (runParser "42" parseSingleExpr)
        (Right (IntLit 42))

    it "boolean literal" do
      shouldEqual
        (runParser "true" parseSingleExpr)
        (Right $ BoolLit true)
      shouldEqual
        (runParser "false" parseSingleExpr)
        (Right $ BoolLit false)

    it "null literal" do
      shouldEqual
        (runParser "null" parseSingleExpr)
        (Right NullLit)

    it "variable" do
      shouldEqual
        (runParser "foo" parseSingleExpr)
        (Right $ Var "foo")

    it "addition" do
      shouldEqual
        (runParser "42 + 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) AddOp (IntLit 53))

    it "subtraction" do
      shouldEqual
        (runParser "42 - 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) SubOp (IntLit 53))

    it "multiplication" do
      shouldEqual
        (runParser "42 * 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) MulOp (IntLit 53))

    it "division" do
      shouldEqual
        (runParser "42 / 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) DivOp (IntLit 53))

    it "modulo operation" do
      shouldEqual
        (runParser "42 % 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) ModOp (IntLit 53))

    it "power" do
      shouldEqual
        (runParser "42 ** 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) PowOp (IntLit 53))

    it "equal" do
      shouldEqual
        (runParser "42 == 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) EqOp (IntLit 53))

    it "not equal" do
      shouldEqual
        (runParser "42 != 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) NotEqOp (IntLit 53))

    it "greeter than" do
      shouldEqual
        (runParser "42 > 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) GTOp (IntLit 53))

    it "less than" do
      shouldEqual
        (runParser "42 < 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) LTOp (IntLit 53))

    it "greeter than or equal" do
      shouldEqual
        (runParser "42 >= 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) GEOp (IntLit 53))

    it "less than or equal" do
      shouldEqual
        (runParser "42 <= 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) LEOp (IntLit 53))

    it "and" do
      shouldEqual
        (runParser "42 && 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) AndOp (IntLit 53))

    it "or" do
      shouldEqual
        (runParser "42 || 53" parseSingleExpr)
        (Right $ BinExpr (IntLit 42) OrOp (IntLit 53))

    it "parenthesized expression" do
      let
        lhs = BinExpr (IntLit 42) AddOp (IntLit 13)
        rhs = IntLit 53
      shouldEqual
        (runParser "(42 + 13) * 53" parseSingleExpr)
        (Right $ BinExpr lhs MulOp rhs)

  describe "statement" do
    it "variable statement" do
      shouldEqual
        (runParser "var foo = 42;" parseStmts)
        (Right [ VarStmt "foo" $ IntLit 42 ])

    it "variable statement (default value)" do
      shouldEqual
        (runParser "var foo;" parseStmts)
        (Right [ VarStmt "foo" NullLit ])

    it "if statement (then only)" do
      let
        input =
          """
          if foo > 10 {
              var bar = 1;
          }
          """
        cond = BinExpr (Var "foo") GTOp (IntLit 10)
        thenBody = [ VarStmt "bar" $ IntLit 1 ]
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
        thenBody = [ VarStmt "bar" $ IntLit 1 ]
        elseBody = [ VarStmt "bar" $ IntLit 2 ]
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
        thenBody = [ VarStmt "bar" $ IntLit 1 ]
        elseIfCond = BinExpr (Var "foo") GTOp (IntLit 5)
        elseIfThenBody = [ VarStmt "bar" $ IntLit 2 ]
        elseIfElseBody = [ VarStmt "bar" $ IntLit 3 ]
        elseBody = [ IfStmt elseIfCond elseIfThenBody $ Just elseIfElseBody ]
      shouldEqual
        (runParser input parseStmts)
        (Right [ IfStmt cond thenBody $ Just elseBody ])
