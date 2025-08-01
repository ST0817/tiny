module Tiny.Evaluation (evalExpr, evalStmt, runEvaluator) where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, get, modify_, put, runStateT)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Int (pow)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))

type Scope = Map String Expr

type Evaluator = StateT Scope (Except String)

runEvaluator :: forall a. Evaluator a -> Scope -> Either String (a /\ Scope)
runEvaluator evaluator scope = runExcept $ runStateT evaluator scope

evalExpr :: Expr -> Evaluator Expr
evalExpr intLit@(IntLit _) = pure intLit
evalExpr boolLit@(BoolLit _) = pure boolLit
evalExpr nullLit@NullLit = pure nullLit
evalExpr (Var name) = do
  scope <- get
  case lookup name scope of
    Just value -> pure value
    Nothing -> throwError $ "Undefined variable: " <> name
evalExpr (BinExpr lhs op rhs) = do
  lhsResult <- evalExpr lhs
  rhsResult <- evalExpr rhs
  case lhsResult /\ rhsResult of
    IntLit lval /\ IntLit rval -> case op of
      AddOp -> pure $ IntLit $ lval + rval
      SubOp -> pure $ IntLit $ lval - rval
      MulOp -> pure $ IntLit $ lval * rval
      DivOp -> pure $ IntLit $ lval / rval
      ModOp -> pure $ IntLit $ lval `mod` rval
      PowOp -> pure $ IntLit $ lval `pow` rval
      EqOp -> pure $ BoolLit $ lval == rval
      NotEqOp -> pure $ BoolLit $ lval /= rval
      GTOp -> pure $ BoolLit $ lval > rval
      LTOp -> pure $ BoolLit $ lval < rval
      GEOp -> pure $ BoolLit $ lval >= rval
      LEOp -> pure $ BoolLit $ lval <= rval
      _ -> throwError "Invalid operation."
    BoolLit lval /\ BoolLit rval -> case op of
      AndOp -> pure $ BoolLit $ lval && rval
      OrOp -> pure $ BoolLit $ lval || rval
      _ -> throwError "Invalid operation."
    _ -> throwError "Invalid operation."

evalInNewScope :: forall a. Evaluator a -> Evaluator a
evalInNewScope evaluator = do
  scope <- get
  result <- evaluator
  put scope
  pure result

evalStmt :: Stmt -> Evaluator Unit
evalStmt (VarStmt name value) = do
  scope <- get
  case lookup name scope of
    Nothing -> modify_ $ insert name value
    Just _ -> throwError $ "Redefined variable: " <> name
evalStmt (IfStmt cond thenBody maybeElseBody) = do
  condResult <- evalExpr cond
  case condResult /\ maybeElseBody of
    (BoolLit true /\ _) -> evalInNewScope $ traverse_ evalStmt thenBody
    (BoolLit false /\ Just elseBody) -> evalInNewScope $ traverse_ evalStmt elseBody
    (BoolLit false /\ Nothing) -> pure unit
    _ -> throwError "Invalid operation."
