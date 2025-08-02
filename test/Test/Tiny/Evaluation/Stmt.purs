module Test.Tiny.Evaluation.Stmt (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..), Pattern(..), Stmt(..))
import Tiny.Evaluation (empty, evalStmt, runEvaluator, singleton)
import Tiny.Object (Object(..))

spec :: Spec Unit
spec = describe "statement" do
  -- var foo = 42;
  it "variable statement" do
    shouldEqual
      (runEvaluator (evalStmt $ VarStmt (VarPattern "foo") (IntLit 42)) empty)
      (Right $ unit /\ singleton "foo" (IntObj 42))

  -- foo = 53;
  it "assignment statement" do
    let
      beforeScope = singleton "foo" $ IntObj 42
    shouldEqual
      (runEvaluator (evalStmt $ AssignStmt (VarPattern "foo") (IntLit 53)) beforeScope)
      (Right $ unit /\ singleton "foo" (IntObj 53))

  -- if 20 > 10 {
  --     var bar = 1;
  -- }
  it "if statement (then only)" do
    let
      cond = BinExpr (IntLit 20) GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
    shouldEqual
      (runEvaluator (evalStmt $ IfStmt cond thenBody Nothing) empty)
      (Right $ unit /\ empty)

  -- if 20 > 10 {
  --     var bar = 1;
  -- } else {
  --     var bar = 2;
  -- }
  it "if statement (then and else)" do
    let
      cond = BinExpr (IntLit 20) GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
      elseBody = [ VarStmt (VarPattern "bar") (IntLit 2) ]
    shouldEqual
      (runEvaluator (evalStmt $ IfStmt cond thenBody $ Just elseBody) empty)
      (Right $ unit /\ empty)

  -- if 20 > 10 {
  --     var bar = 1;
  --  } else if 20 > 5 {
  --     var bar = 2;
  -- } else {
  --     var bar = 3;
  -- }
  it "if statement (else if)" do
    let
      cond = BinExpr (IntLit 20) GTOp (IntLit 10)
      thenBody = [ VarStmt (VarPattern "bar") (IntLit 1) ]
      elseIfCond = BinExpr (Var "foo") GTOp (IntLit 5)
      elseIfThenBody = [ VarStmt (VarPattern "bar") (IntLit 2) ]
      elseIfElseBody = [ VarStmt (VarPattern "bar") (IntLit 3) ]
      elseBody = [ IfStmt elseIfCond elseIfThenBody $ Just elseIfElseBody ]
    shouldEqual
      (runEvaluator (evalStmt $ IfStmt cond thenBody $ Just elseBody) empty)
      (Right $ unit /\ empty)

  -- var bar;
  -- if true {
  --     bar = 1;
  -- }
  it "nested scope" do
    let
      barDef = VarStmt (VarPattern "bar") NullLit
      cond = BoolLit true
      thenBody = [ AssignStmt (VarPattern "bar") (IntLit 1) ]
      ifStmt = IfStmt cond thenBody Nothing
      stmts = [ barDef, ifStmt ]
      afterScope = singleton "bar" $ IntObj 1
    shouldEqual
      (runEvaluator (traverse_ evalStmt stmts) empty)
      (Right $ unit /\ afterScope)
