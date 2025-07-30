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
  | OrPrec
  | AndPrec
  | CompPrec
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

parseBoolLit :: TinyParser Expr
parseBoolLit = symbol "true" $> BoolLit true
  <|> symbol "false" $> BoolLit false

parseTerm :: TinyParser Expr
parseTerm = parseIntLit <|> parseBoolLit

binOp :: String -> BinOp -> Prec -> Boolean -> TinyParser { op :: BinOp, opPrec :: Prec, isRightAssoc :: Boolean }
binOp str op opPrec isRightAssoc = symbol str $> { op, opPrec, isRightAssoc }

parseExpr :: Prec -> TinyParser Expr
parseExpr prec = do
  first <- parseTerm
  rest <- many $ try do
    { op, opPrec, isRightAssoc } <-
      binOp "**" PowOp ProdPrec true
        <|> binOp "==" EqOp CompPrec false
        <|> binOp "!=" NotEqOp CompPrec false
        <|> binOp ">=" GEOp CompPrec false
        <|> binOp "<=" LEOp CompPrec false
        <|> binOp "&&" AndOp AndPrec false
        <|> binOp "||" OrOp OrPrec false
        <|> binOp "+" AddOp SumPrec false
        <|> binOp "-" SubOp SumPrec false
        <|> binOp "*" MulOp ProdPrec false
        <|> binOp "/" DivOp ProdPrec false
        <|> binOp "%" ModOp ProdPrec false
        <|> binOp ">" GTOp CompPrec false
        <|> binOp "<" LTOp CompPrec false
    guard $ opPrec > prec || isRightAssoc
    rhs <- parseExpr opPrec
    pure { op, rhs }
  pure $ foldl (\lhs { op, rhs } -> BinExpr lhs op rhs) first rest
