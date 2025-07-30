module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Tiny.Evaluation as Evaluation
import Test.Tiny.Parsing as Parsing

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "tiny" do
    Parsing.spec
    Evaluation.spec
