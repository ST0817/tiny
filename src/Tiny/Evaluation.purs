module Tiny.Evaluation (Scope, evalExpr, evalStmt, runEvaluator) where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Either (Either)
import Data.Int (pow)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))

type Scope = Map String Expr

type Evaluator = StateT Scope (Except String)

runEvaluator :: forall a. Evaluator a -> Scope -> Either String (Tuple a Scope)
runEvaluator evaluator scope = runExcept $ runStateT evaluator scope

evalExpr :: Expr -> Evaluator Expr
evalExpr intLit@(IntLit _) = pure intLit
evalExpr boolLit@(BoolLit _) = pure boolLit
evalExpr (Var name) = do
  scope <- get
  case lookup name scope of
    Just value -> pure value
    Nothing -> throwError $ "Undefined variable: " <> name
evalExpr (BinExpr lhs op rhs) = do
  lhsResult <- evalExpr lhs
  rhsResult <- evalExpr rhs
  case { lhsResult, rhsResult } of
    { lhsResult: (IntLit lval), rhsResult: (IntLit rval) } -> case op of
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
    { lhsResult: (BoolLit lval), rhsResult: (BoolLit rval) } -> case op of
      AndOp -> pure $ BoolLit $ lval && rval
      OrOp -> pure $ BoolLit $ lval || rval
      _ -> throwError "Invalid operation."
    _ -> throwError "Invalid operation."

evalStmt :: Stmt -> Evaluator Unit
evalStmt (VarStmt name value) = do
  scope <- get
  case lookup name scope of
    Nothing -> modify_ $ insert name value
    Just _ -> throwError $ "Redefined variable: " <> name
