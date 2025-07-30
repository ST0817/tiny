module Tiny.Parsing (OpPrec(..), parseExpr) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (foldl)
import Data.Tuple (Tuple(..))
import Parsing (Parser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (BinOp(..), Expr(..))

data OpPrec
  = LowestPrec
  | SumPrec
  | ProdPrec

derive instance Eq OpPrec
derive instance Ord OpPrec

type TinyParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

symbol :: String -> TinyParser String
symbol = tokenParser.symbol

parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer

parseBinOp :: TinyParser (Tuple BinOp OpPrec)
parseBinOp = symbol "+" $> Tuple AddOp SumPrec
  <|> symbol "-" $> Tuple SubOp SumPrec
  <|> symbol "*" $> Tuple MulOp ProdPrec
  <|> symbol "/" $> Tuple DivOp ProdPrec

parseExpr :: OpPrec -> TinyParser Expr
parseExpr prevOpPrec = do
  expr <- parseIntLit
  rest <- many $ try do
    Tuple op opPrec <- parseBinOp
    guard $ opPrec > prevOpPrec
    rhs <- parseExpr opPrec
    pure $ Tuple op rhs
  pure $ foldl (\lhs (Tuple op rhs) -> BinExpr lhs op rhs) expr rest
