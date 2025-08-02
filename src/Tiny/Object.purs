module Tiny.Object (Object(..)) where

import Prelude

import Data.Array (intercalate)

data Object
  = FloatObj Number
  | IntObj Int
  | BoolObj Boolean
  | NullObj
  | TupleObj (Array Object)

derive instance Eq Object

instance Show Object where
  show (FloatObj value) = show value
  show (IntObj value) = show value
  show (BoolObj value) = show value
  show NullObj = "null"
  show (TupleObj elems) =
    "(" <> intercalate ", " (show <$> elems) <> ")"
