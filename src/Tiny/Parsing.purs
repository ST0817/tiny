module Tiny.Parsing (Prec(..), parseExpr) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (foldl)
import Parsing (Parser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (BinOp(..), Expr(..))

data Prec
  = LowestPrec
  | SumPrec
  | ProdPrec

derive instance Eq Prec
derive instance Ord Prec

type TinyParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

symbol :: String -> TinyParser String
symbol = tokenParser.symbol

parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer

parseBinOp :: TinyParser { op :: BinOp, opPrec :: Prec, isRightAssoc :: Boolean }
parseBinOp = symbol "+" $> { op: AddOp, opPrec: SumPrec, isRightAssoc: false }
  <|> symbol "-" $> { op: SubOp, opPrec: SumPrec, isRightAssoc: false }
  <|> symbol "*" $> { op: MulOp, opPrec: ProdPrec, isRightAssoc: false }
  <|> symbol "/" $> { op: DivOp, opPrec: ProdPrec, isRightAssoc: false }
  <|> symbol "^" $> { op: PowOp, opPrec: ProdPrec, isRightAssoc: true }

parseExpr :: Prec -> TinyParser Expr
parseExpr prec = do
  first <- parseIntLit
  rest <- many $ try do
    { op, opPrec, isRightAssoc } <- parseBinOp
    guard $ opPrec > prec || isRightAssoc
    rhs <- parseExpr opPrec
    pure { op, rhs }
  pure $ foldl (\lhs { op, rhs } -> BinExpr lhs op rhs) first rest
