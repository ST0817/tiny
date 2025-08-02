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
