module Test.Tiny.Evaluation (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Map (empty, singleton)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))
import Tiny.Evaluation (evalExpr, evalStmt, runEvaluator)

spec :: Spec Unit
spec = describe "evaluation" do
  describe "expression" do
    it "integer literal" do
      (runEvaluator (evalExpr $ IntLit 42) empty)
        `shouldEqual`
          Right (Tuple (IntLit 42) empty)

    it "boolean literal" do
      (runEvaluator (evalExpr $ BoolLit true) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BoolLit false) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "variable" do
      let scope = singleton "foo" $ IntLit 42
      (runEvaluator (evalExpr $ Var "foo") scope)
        `shouldEqual`
          Right (Tuple (IntLit 42) scope)

    it "addition" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) AddOp (IntLit 53)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 95) empty)

    it "subtraction" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) SubOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 29) empty)

    it "multiplication" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) MulOp (IntLit 6)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 78) empty)

    it "division" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) DivOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 3) empty)

    it "modulo operation" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) ModOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 3) empty)

    it "power" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 4) PowOp (IntLit 3)) empty)
        `shouldEqual`
          Right (Tuple (IntLit 64) empty)

    it "equal" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "not equal" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "greeter than" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) GTOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "less than" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) LTOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "greeter than or equal" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) GEOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "less than or equal" do
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) LEOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 13)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "and" do
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit true)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit false)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)
      (runEvaluator (evalExpr $ BinExpr (BoolLit false) AndOp (BoolLit false)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

    it "or" do
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit true)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit false)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit true) empty)
      (runEvaluator (evalExpr $ BinExpr (BoolLit false) OrOp (BoolLit false)) empty)
        `shouldEqual`
          Right (Tuple (BoolLit false) empty)

  describe "statement" do
    it "variable statement" do
      (runEvaluator (evalStmt $ VarStmt "foo" (IntLit 42)) empty)
        `shouldEqual`
          Right (Tuple unit (singleton "foo" (IntLit 42)))
