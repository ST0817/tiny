module Tiny.Parsing (parseExpr) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, many)
import Data.Tuple (Tuple(..))
import Parsing (Parser)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (BinOp(..), Expr(..))

type TinyParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

symbol :: String -> TinyParser String
symbol = tokenParser.symbol

parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer

parseExpr :: TinyParser Expr
parseExpr = do
  expr <- parseIntLit
  foldl (\lhs (Tuple op rhs) -> BinExpr lhs op rhs) expr <$> many do
    op <- symbol "+" $> AddOp <|> symbol "-" $> SubOp
    rhs <- parseIntLit
    pure $ Tuple op rhs
