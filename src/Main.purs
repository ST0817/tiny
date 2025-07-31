module Main where

import Prelude

import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (empty)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (error, log)
import Effect.Ref (new, read, write)
import Node.EventEmitter (on_)
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Parsing (runParser)
import Parsing.String (parseErrorHuman)
import Tiny.Evaluation (evalExpr, evalStmt, runEvaluator)
import Tiny.Parsing (parseSingleExpr, parseStmts)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt ">> " interface
  scopeRef <- new empty
  prompt interface
  interface # on_ lineH case _ of
    "" -> prompt interface
    ":quit" -> close interface
    input -> do
      case runParser input parseSingleExpr of
        Right expr -> do
          log $ "Ast:"
          logShow expr
          scope <- read scopeRef
          case runEvaluator (evalExpr expr) scope of
            Right (Tuple result _) -> do
              log "Result:"
              logShow result
            Left err -> error err
        Left _ -> case runParser input parseStmts of
          Right stmts -> do
            log $ "Ast:\n" <> (joinWith "\n" $ toArray $ show <$> stmts)
            scope <- read scopeRef
            case runEvaluator (traverse_ evalStmt stmts) scope of
              Right (Tuple _ scope') -> write scope' scopeRef
              Left err -> error err
          Left err -> error $ joinWith "\n" $ parseErrorHuman input 20 err
      prompt interface
