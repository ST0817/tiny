module Test.Tiny.Parsing (spec) where

import Prelude

import Data.Array.NonEmpty (singleton)
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
      (runParser "42" parseSingleExpr)
        `shouldEqual`
          Right (IntLit 42)

    it "boolean literal" do
      (runParser "true" parseSingleExpr)
        `shouldEqual`
          Right (BoolLit true)
      (runParser "false" parseSingleExpr)
        `shouldEqual`
          Right (BoolLit false)

    it "variable" do
      (runParser "foo" parseSingleExpr)
        `shouldEqual`
          Right (Var "foo")

    it "addition" do
      (runParser "42 + 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) AddOp (IntLit 53))

    it "subtraction" do
      (runParser "42 - 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) SubOp (IntLit 53))

    it "multiplication" do
      (runParser "42 * 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) MulOp (IntLit 53))

    it "division" do
      (runParser "42 / 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) DivOp (IntLit 53))

    it "modulo operation" do
      (runParser "42 % 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) ModOp (IntLit 53))

    it "power" do
      (runParser "42 ** 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) PowOp (IntLit 53))

    it "equal" do
      (runParser "42 == 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) EqOp (IntLit 53))

    it "not equal" do
      (runParser "42 != 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) NotEqOp (IntLit 53))

    it "greeter than" do
      (runParser "42 > 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) GTOp (IntLit 53))

    it "less than" do
      (runParser "42 < 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) LTOp (IntLit 53))

    it "greeter than or equal" do
      (runParser "42 >= 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) GEOp (IntLit 53))

    it "less than or equal" do
      (runParser "42 <= 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) LEOp (IntLit 53))

    it "and" do
      (runParser "42 && 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) AndOp (IntLit 53))

    it "or" do
      (runParser "42 || 53" parseSingleExpr)
        `shouldEqual`
          Right (BinExpr (IntLit 42) OrOp (IntLit 53))

  describe "statement" do
    it "variable statement" do
      (runParser "var foo = 42;" parseStmts)
        `shouldEqual`
          Right (singleton $ VarStmt "foo" $ IntLit 42)

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
      (runParser input $ parseStmts)
        `shouldEqual`
          Right (singleton $ IfStmt cond thenBody Nothing)

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
      (runParser input parseStmts)
        `shouldEqual`
          Right (singleton $ IfStmt cond thenBody $ Just elseBody)

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
      (runParser input parseStmts)
        `shouldEqual`
          Right (singleton $ IfStmt cond thenBody $ Just elseBody)
