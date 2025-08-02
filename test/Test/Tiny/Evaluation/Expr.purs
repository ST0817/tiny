module Test.Tiny.Evaluation.Expr (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..))
import Tiny.Evaluation (empty, evalExpr, runEvaluator, singleton)

spec :: Spec Unit
spec = describe "expression" do
  it "integer literal" do
    -- 42
    shouldEqual
      (runEvaluator (evalExpr $ IntLit 42) empty)
      (Right $ IntLit 42 /\ empty)

  it "boolean literal" do
    -- true
    shouldEqual
      (runEvaluator (evalExpr $ BoolLit true) empty)
      (Right $ BoolLit true /\ empty)
    -- false
    shouldEqual
      (runEvaluator (evalExpr $ BoolLit false) empty)
      (Right $ BoolLit false /\ empty)

  it "null literal" do
    -- foo
    shouldEqual
      (runEvaluator (evalExpr NullLit) empty)
      (Right $ NullLit /\ empty)

  it "variable" do
    -- foo
    -- -> 42
    let scope = singleton "foo" $ IntLit 42
    shouldEqual
      (runEvaluator (evalExpr $ Var "foo") scope)
      (Right $ IntLit 42 /\ scope)

  it "addition" do
    -- 42 + 53
    -- -> 95
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) AddOp (IntLit 53)) empty)
      (Right $ IntLit 95 /\ empty)

  it "subtraction" do
    -- 42 - 13
    -- -> 29
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) SubOp (IntLit 13)) empty)
      (Right $ IntLit 29 /\ empty)

  it "multiplication" do
    -- 13 * 6
    -- -> 78
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) MulOp (IntLit 6)) empty)
      (Right $ IntLit 78 /\ empty)

  it "division" do
    -- 42 / 13
    -- -> 3
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) DivOp (IntLit 13)) empty)
      (Right $ IntLit 3 /\ empty)

  it "modulo operation" do
    -- 42 % 13
    -- -> 3
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) ModOp (IntLit 13)) empty)
      (Right $ IntLit 3 /\ empty)

  it "power" do
    -- 4 ** 3
    -- -> 64
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 4) PowOp (IntLit 3)) empty)
      (Right $ IntLit 64 /\ empty)

  it "equal" do
    -- 42 == 42
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 42)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 == 13
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) EqOp (IntLit 13)) empty)
      (Right $ BoolLit false /\ empty)

  it "not equal" do
    -- 42 != 13
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 13)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 != 42
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) NotEqOp (IntLit 42)) empty)
      (Right $ BoolLit false /\ empty)

  it "greeter than" do
    -- 42 > 13
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 13)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 > 42
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GTOp (IntLit 42)) empty)
      (Right $ BoolLit false /\ empty)
    -- 13 > 42
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) GTOp (IntLit 42)) empty)
      (Right $ BoolLit false /\ empty)

  it "less than" do
    -- 13 < 42
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) LTOp (IntLit 42)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 < 42
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 42)) empty)
      (Right $ BoolLit false /\ empty)
    -- 42 < 13
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LTOp (IntLit 13)) empty)
      (Right $ BoolLit false /\ empty)

  it "greeter than or equal" do
    -- 42 >= 13
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 13)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 >= 42
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) GEOp (IntLit 42)) empty)
      (Right $ BoolLit true /\ empty)
    -- 13 >= 42
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) GEOp (IntLit 42)) empty)
      (Right $ BoolLit false /\ empty)

  it "less than or equal" do
    -- 13 <= 42
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 13) LEOp (IntLit 42)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 <= 42
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 42)) empty)
      (Right $ BoolLit true /\ empty)
    -- 42 <= 13
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (IntLit 42) LEOp (IntLit 13)) empty)
      (Right $ BoolLit false /\ empty)

  it "and" do
    -- true && true
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit true)) empty)
      (Right $ BoolLit true /\ empty)
    -- true && false
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) AndOp (BoolLit false)) empty)
      (Right $ BoolLit false /\ empty)
    -- false && false
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit false) AndOp (BoolLit false)) empty)
      (Right $ BoolLit false /\ empty)

  it "or" do
    -- true || true
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit true)) empty)
      (Right $ BoolLit true /\ empty)
    -- true || false
    -- -> true
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit true) OrOp (BoolLit false)) empty)
      (Right $ BoolLit true /\ empty)
    -- false || false
    -- -> false
    shouldEqual
      (runEvaluator (evalExpr $ BinExpr (BoolLit false) OrOp (BoolLit false)) empty)
      (Right $ BoolLit false /\ empty)
