module Tiny.Evaluation
  ( Scope(..)
  , empty
  , evalExpr
  , evalStmt
  , fromArray
  , runEvaluator
  , singleton
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array (length, zip)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Tiny.Ast (BinOp(..), Expr(..), Pattern(..), Stmt(..))
import Tiny.Object (Object(..))

newtype Scope = Scope
  { vars :: Map String Object
  , outer :: Maybe Scope
  }

derive instance Eq Scope
derive instance Generic Scope _
instance Show Scope where
  show = defer \_ -> genericShow

empty :: Scope
empty = Scope { vars: Map.empty, outer: Nothing }

singleton :: String -> Object -> Scope
singleton name object =
  Scope { vars: Map.singleton name object, outer: Nothing }

fromArray :: Array (String /\ Object) -> Scope
fromArray elems =
  Scope { vars: Map.fromFoldable elems, outer: Nothing }

lookup :: String -> Scope -> Maybe Object
lookup name (Scope scope) =
  (Map.lookup name scope.vars) <|> (lookup name =<< scope.outer)

insert :: String -> Object -> Scope -> Scope
insert name object (Scope scope) =
  Scope $ scope { vars = Map.insert name object scope.vars }

update :: String -> Object -> Scope -> Scope
update name object (Scope scope) =
  fromMaybe
    (Scope $ scope { vars = Map.update (\_ -> Just object) name scope.vars })
    (update name object <$> scope.outer)

enterScope :: Scope -> Scope
enterScope outer = Scope $ { vars: Map.empty, outer: Just outer }

leaveScope :: Scope -> Scope
leaveScope (Scope { outer: Just outer }) = outer
leaveScope scope = scope

type Evaluator = StateT Scope (Except String)

runEvaluator :: forall a. Evaluator a -> Scope -> Either String (a /\ Scope)
runEvaluator evaluator scope = runExcept $ runStateT evaluator scope

getVar :: String -> Evaluator Object
getVar name = do
  scope <- get
  case lookup name scope of
    Just object -> pure object
    Nothing -> throwError $ "Undefined variable: " <> name

checkUndefinedVar :: String -> Evaluator Unit
checkUndefinedVar name = do
  scope <- get
  case lookup name scope of
    Nothing -> pure unit
    Just _ -> throwError $ "Redefined variable: " <> name

evalExpr :: Expr -> Evaluator Object
evalExpr (FloatLit value) = pure $ FloatObj value
evalExpr (IntLit value) = pure $ IntObj value
evalExpr (BoolLit value) = pure $ BoolObj value
evalExpr NullLit = pure NullObj
evalExpr (Var name) = getVar name
evalExpr (TupleExpr [ elem ]) = evalExpr elem
evalExpr (TupleExpr elems) = TupleObj <$> (traverse evalExpr elems)
evalExpr (BinExpr lhs op rhs) = do
  lhsResult <- evalExpr lhs
  rhsResult <- evalExpr rhs
  case lhsResult, rhsResult of
    FloatObj lval, FloatObj rval -> case op of
      AddOp -> pure $ FloatObj $ lval + rval
      SubOp -> pure $ FloatObj $ lval - rval
      MulOp -> pure $ FloatObj $ lval * rval
      DivOp -> pure $ FloatObj $ lval / rval
      ModOp -> pure $ FloatObj $ lval `mod` rval
      PowOp -> pure $ FloatObj $ lval `Number.pow` rval
      EqOp -> pure $ BoolObj $ lval == rval
      NotEqOp -> pure $ BoolObj $ lval /= rval
      GTOp -> pure $ BoolObj $ lval > rval
      LTOp -> pure $ BoolObj $ lval < rval
      GEOp -> pure $ BoolObj $ lval >= rval
      LEOp -> pure $ BoolObj $ lval <= rval
      _ -> throwError "Invalid operation."
    IntObj lval, IntObj rval -> case op of
      AddOp -> pure $ IntObj $ lval + rval
      SubOp -> pure $ IntObj $ lval - rval
      MulOp -> pure $ IntObj $ lval * rval
      DivOp -> pure $ IntObj $ lval / rval
      ModOp -> pure $ IntObj $ lval `mod` rval
      PowOp -> pure $ IntObj $ lval `Int.pow` rval
      EqOp -> pure $ BoolObj $ lval == rval
      NotEqOp -> pure $ BoolObj $ lval /= rval
      GTOp -> pure $ BoolObj $ lval > rval
      LTOp -> pure $ BoolObj $ lval < rval
      GEOp -> pure $ BoolObj $ lval >= rval
      LEOp -> pure $ BoolObj $ lval <= rval
      _ -> throwError "Invalid operation."
    BoolObj lval, BoolObj rval -> case op of
      AndOp -> pure $ BoolObj $ lval && rval
      OrOp -> pure $ BoolObj $ lval || rval
      _ -> throwError "Invalid operation."
    _, _ -> throwError "Invalid operation."

evalBlock :: Array Stmt -> Evaluator Unit
evalBlock body = do
  modify_ enterScope
  result <- traverse_ evalStmt body
  modify_ leaveScope
  pure result

defineVar :: String -> Expr -> Evaluator Unit
defineVar name expr = do
  checkUndefinedVar name
  object <- evalExpr expr
  modify_ $ insert name object

updateVar :: String -> Expr -> Evaluator Unit
updateVar name expr = do
  _ <- getVar name
  object <- evalExpr expr
  modify_ $ update name object

evalStmt :: Stmt -> Evaluator Unit
evalStmt (VarStmt pattern expr) =
  case pattern, expr of
    VarPattern name, _ -> defineVar name expr
    TuplePattern [ elemPattern ], TupleExpr [ elemExpr ] ->
      evalStmt $ VarStmt elemPattern elemExpr
    TuplePattern patterns, TupleExpr elems
      | length patterns == length elems ->
          traverse_
            ( \(elemPattern /\ elemExpr) ->
                evalStmt $ VarStmt elemPattern elemExpr
            )
            (zip patterns elems)
    _, _ -> throwError "Invalid variable definition."
evalStmt (AssignStmt pattern expr) =
  case pattern, expr of
    VarPattern name, _ -> updateVar name expr
    TuplePattern [ elemPattern ], TupleExpr [ elemExpr ] ->
      evalStmt $ AssignStmt elemPattern elemExpr
    TuplePattern patterns, TupleExpr elems
      | length patterns == length elems ->
          traverse_
            ( \(elemPattern /\ elemExpr) ->
                evalStmt $ AssignStmt elemPattern elemExpr
            )
            (zip patterns elems)
    _, _ -> throwError "Invalid variable assignment."
evalStmt (IfStmt cond thenBody maybeElseBody) = do
  condResult <- evalExpr cond
  case condResult /\ maybeElseBody of
    (BoolObj true /\ _) -> evalBlock thenBody
    (BoolObj false /\ Just elseBody) -> evalBlock elseBody
    (BoolObj false /\ Nothing) -> pure unit
    _ -> throwError "Invalid operation."
