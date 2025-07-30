module Tiny.Evaluation (evalExpr, runEvaluator) where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Either (Either)
import Data.Int (pow)
import Data.Tuple (Tuple(..))
import Tiny.Ast (BinOp(..), Expr(..))

type Evaluator = Except String

runEvaluator :: forall a. Evaluator a -> Either String a
runEvaluator = runExcept

evalExpr :: Expr -> Evaluator Expr
evalExpr intLit@(IntLit _) = pure intLit
evalExpr (BinExpr lhs op rhs) = do
  lhsExpr <- evalExpr lhs
  rhsExpr <- evalExpr rhs
  case Tuple lhsExpr rhsExpr of
    Tuple (IntLit lval) (IntLit rval) -> pure <<< IntLit $ case op of
      AddOp -> lval + rval
      SubOp -> lval - rval
      MulOp -> lval * rval
      DivOp -> lval / rval
      PowOp -> lval `pow` rval
    _ -> throwError "Invalid operation."
