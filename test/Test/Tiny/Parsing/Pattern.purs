module Test.Tiny.Parsing.Pattern (spec) where

import Prelude

import Data.Either (Either(..))
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (Pattern(..))
import Tiny.Parsing (parsePattern)

spec :: Spec Unit
spec = describe "pattern" do
  it "variable pattern" do
    shouldEqual
      (runParser "foo" parsePattern)
      (Right $ VarPattern "foo")

  it "tuple pattern" do
    shouldEqual
      (runParser "(foo, bar)" parsePattern)
      (Right $ TuplePattern [ VarPattern "foo", VarPattern "bar" ])

  it "tuple pattern (nested)" do
    let nested = TuplePattern [ VarPattern "bar", VarPattern "hoge" ]
    shouldEqual
      (runParser "(foo, (bar, hoge))" parsePattern)
      (Right $ TuplePattern [ VarPattern "foo", nested ])
