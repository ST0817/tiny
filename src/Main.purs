module Main where

import Prelude

import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (error, log)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (runParser)
import Parsing.String (parseErrorHuman)
import Tiny.Evaluation (eval)
import Tiny.Parsing (parseIntLit)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt ">> " interface
  prompt interface
  interface # on_ lineH case _ of
    "" -> prompt interface
    ":quit" -> close interface
    input -> do
      case runParser input parseIntLit of
        Right expr -> do
          log $ "Ast: " <> show expr
          logShow $ eval expr
        Left err -> error $ joinWith "\n" $ parseErrorHuman input 20 err
      prompt interface
