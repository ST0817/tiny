module Test.Tiny.Evaluation (spec) where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..))
import Tiny.Evaluation (evalExpr, runEvaluator)

spec :: Spec Unit
spec = describe "evaluation" do
  it "integer literal" do
    (runEvaluator $ evalExpr $ IntLit 42)
      `shouldEqual`
        Right (IntLit 42)

  it "boolean literal" do
    (runEvaluator $ evalExpr $ BoolLit true)
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BoolLit false)
      `shouldEqual`
        Right (BoolLit false)

  it "addition" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) AddOp (IntLit 53))
      `shouldEqual`
        Right (IntLit 95)

  it "subtraction" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) SubOp (IntLit 13))
      `shouldEqual`
        Right (IntLit 29)

  it "multiplication" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 13) MulOp (IntLit 6))
      `shouldEqual`
        Right (IntLit 78)

  it "division" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) DivOp (IntLit 13))
      `shouldEqual`
        Right (IntLit 3)

  it "modulo operation" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) ModOp (IntLit 13))
      `shouldEqual`
        Right (IntLit 3)

  it "power" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 4) PowOp (IntLit 3))
      `shouldEqual`
        Right (IntLit 64)

  it "equal" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit false)

  it "not equal" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit false)

  it "greeter than" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit false)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 13) GTOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit false)

  it "less than" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 13) LTOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit false)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit false)

  it "greeter than or equal" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 13) GEOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit false)

  it "less than or equal" do
    (runEvaluator $ evalExpr $ BinExpr (IntLit 13) LEOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 42))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 13))
      `shouldEqual`
        Right (BoolLit false)

  it "and" do
    (runEvaluator $ evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit true))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit false))
      `shouldEqual`
        Right (BoolLit false)
    (runEvaluator $ evalExpr $ BinExpr (BoolLit false) AndOp (BoolLit false))
      `shouldEqual`
        Right (BoolLit false)

  it "or" do
    (runEvaluator $ evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit true))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit false))
      `shouldEqual`
        Right (BoolLit true)
    (runEvaluator $ evalExpr $ BinExpr (BoolLit false) OrOp (BoolLit false))
      `shouldEqual`
        Right (BoolLit false)
