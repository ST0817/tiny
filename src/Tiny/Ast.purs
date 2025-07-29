module Tiny.Ast (Expr(..)) where

import Prelude

data Expr = IntLit Int

instance Show Expr where
  show (IntLit value) = show value
