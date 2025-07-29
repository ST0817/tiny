module Tiny.Evaluation (eval) where

import Tiny.Ast (Expr(..))

eval :: Expr -> Expr
eval intLit@(IntLit _) = intLit
