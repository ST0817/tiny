module Test.Tiny.Parsing (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Tiny.Parsing.Expr as Expr
import Test.Tiny.Parsing.Stmt as Stmt

spec :: Spec Unit
spec = describe "parsing" do
  Expr.spec
  Stmt.spec
