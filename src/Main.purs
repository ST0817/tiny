module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt ">> " interface
  prompt interface
  interface # on_ lineH case _ of
    "" -> prompt interface
    ":quit" -> close interface
    input -> do
      log input
      prompt interface
