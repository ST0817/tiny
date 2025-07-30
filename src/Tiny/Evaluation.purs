module Tiny.Evaluation (evalExpr, runEvaluator) where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Either (Either)
import Data.Int (pow)
import Tiny.Ast (BinOp(..), Expr(..))

type Evaluator = Except String

runEvaluator :: forall a. Evaluator a -> Either String a
runEvaluator = runExcept

evalExpr :: Expr -> Evaluator Expr
evalExpr intLit@(IntLit _) = pure intLit
evalExpr boolLit@(BoolLit _) = pure boolLit
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
