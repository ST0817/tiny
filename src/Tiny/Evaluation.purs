module Tiny.Evaluation
  ( Scope(..)
  , empty
  , evalExpr
  , evalStmt
  , runEvaluator
  , singleton
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))

newtype Scope = Scope
  { vars :: Map String Expr
  , outer :: Maybe Scope
  }

derive instance Eq Scope
derive instance Generic Scope _
instance Show Scope where
  show = defer \_ -> genericShow

empty :: Scope
empty = Scope { vars: Map.empty, outer: Nothing }

singleton :: String -> Expr -> Scope
singleton name value =
  Scope { vars: Map.singleton name value, outer: Nothing }

lookup :: String -> Scope -> Maybe Expr
lookup name (Scope scope) =
  (Map.lookup name scope.vars) <|> (lookup name =<< scope.outer)

insert :: String -> Expr -> Scope -> Scope
insert name value (Scope scope) =
  Scope $ scope { vars = Map.insert name value scope.vars }

update :: String -> Expr -> Scope -> Scope
update name value (Scope scope) =
  fromMaybe
    (Scope $ scope { vars = Map.update (\_ -> Just value) name scope.vars })
    (update name value <$> scope.outer)

enterScope :: Scope -> Scope
enterScope outer = Scope $ { vars: Map.empty, outer: Just outer }

leaveScope :: Scope -> Scope
leaveScope (Scope { outer: Just outer }) = outer
leaveScope scope = scope

type Evaluator = StateT Scope (Except String)

runEvaluator :: forall a. Evaluator a -> Scope -> Either String (a /\ Scope)
runEvaluator evaluator scope = runExcept $ runStateT evaluator scope

getVar :: String -> Evaluator Expr
getVar name = do
  scope <- get
  case lookup name scope of
    Just value -> pure value
    Nothing -> throwError $ "Undefined variable: " <> name

evalExpr :: Expr -> Evaluator Expr
evalExpr floatLit@(FloatLit _) = pure floatLit
evalExpr intLit@(IntLit _) = pure intLit
evalExpr boolLit@(BoolLit _) = pure boolLit
evalExpr nullLit@NullLit = pure nullLit
evalExpr (Var name) = getVar name
evalExpr (BinExpr lhs op rhs) = do
  lhsResult <- evalExpr lhs
  rhsResult <- evalExpr rhs
  case lhsResult, rhsResult of
    FloatLit lval, FloatLit rval -> case op of
      AddOp -> pure $ FloatLit $ lval + rval
      SubOp -> pure $ FloatLit $ lval - rval
      MulOp -> pure $ FloatLit $ lval * rval
      DivOp -> pure $ FloatLit $ lval / rval
      ModOp -> pure $ FloatLit $ lval `mod` rval
      PowOp -> pure $ FloatLit $ lval `Number.pow` rval
      EqOp -> pure $ BoolLit $ lval == rval
      NotEqOp -> pure $ BoolLit $ lval /= rval
      GTOp -> pure $ BoolLit $ lval > rval
      LTOp -> pure $ BoolLit $ lval < rval
      GEOp -> pure $ BoolLit $ lval >= rval
      LEOp -> pure $ BoolLit $ lval <= rval
      _ -> throwError "Invalid operation."
    IntLit lval, IntLit rval -> case op of
      AddOp -> pure $ IntLit $ lval + rval
      SubOp -> pure $ IntLit $ lval - rval
      MulOp -> pure $ IntLit $ lval * rval
      DivOp -> pure $ IntLit $ lval / rval
      ModOp -> pure $ IntLit $ lval `mod` rval
      PowOp -> pure $ IntLit $ lval `Int.pow` rval
      EqOp -> pure $ BoolLit $ lval == rval
      NotEqOp -> pure $ BoolLit $ lval /= rval
      GTOp -> pure $ BoolLit $ lval > rval
      LTOp -> pure $ BoolLit $ lval < rval
      GEOp -> pure $ BoolLit $ lval >= rval
      LEOp -> pure $ BoolLit $ lval <= rval
      _ -> throwError "Invalid operation."
    BoolLit lval, BoolLit rval -> case op of
      AndOp -> pure $ BoolLit $ lval && rval
      OrOp -> pure $ BoolLit $ lval || rval
      _ -> throwError "Invalid operation."
    _, _ -> throwError "Invalid operation."

evalBlock :: Array Stmt -> Evaluator Unit
evalBlock body = do
  modify_ enterScope
  result <- traverse_ evalStmt body
  modify_ leaveScope
  pure result

evalStmt :: Stmt -> Evaluator Unit
evalStmt (VarStmt name value) = do
  scope <- get
  case lookup name scope of
    Nothing -> modify_ $ insert name value
    Just _ -> throwError $ "Redefined variable: " <> name
evalStmt (AssignStmt name value) =
  getVar name *> modify_ (update name value)
evalStmt (IfStmt cond thenBody maybeElseBody) = do
  condResult <- evalExpr cond
  case condResult /\ maybeElseBody of
    (BoolLit true /\ _) -> evalBlock thenBody
    (BoolLit false /\ Just elseBody) -> evalBlock elseBody
    (BoolLit false /\ Nothing) -> pure unit
    _ -> throwError "Invalid operation."
