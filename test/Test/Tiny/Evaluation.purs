module Test.Tiny.Evaluation (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Tiny.Evaluation.Expr as Expr
import Test.Tiny.Evaluation.Stmt as Stmt

spec :: Spec Unit
spec = describe "evaluation" do
  Expr.spec
  Stmt.spec
