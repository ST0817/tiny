module Tiny.Parsing (parseSingleExpr, parseStmts) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray)
import Parsing (Parser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many, many1)
import Parsing.Language (emptyDef)
import Parsing.String (eof)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (BinOp(..), Expr(..), Stmt(..))

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

-- Ident = [a-zA-Z]+
ident :: TinyParser String
ident = tokenParser.identifier

-- IntLit = [0-9]+
parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer

-- BoolLit = "true" | "false"
parseBoolLit :: TinyParser Expr
parseBoolLit =
  symbol "true" $> BoolLit true
    <|> symbol "false" $> BoolLit false

-- Var = Ident
parseVar :: TinyParser Expr
parseVar = Var <$> ident

-- Term
--   = IntLit
--   | BoolLit
--   | Var
parseTerm :: TinyParser Expr
parseTerm =
  parseIntLit
    <|> parseBoolLit
    <|> parseVar

-- BinOp
--   = "+" | "-" | "*" | "/" | "%" | "**"
--   | "==" | "!=" | ">" | "<" | ">=" | "<="
--   | "&&" | "||"
binOp :: String -> BinOp -> Prec -> Boolean -> TinyParser { op :: BinOp, opPrec :: Prec, isRightAssoc :: Boolean }
binOp str op opPrec isRightAssoc = symbol str $> { op, opPrec, isRightAssoc }

-- Expr
--   = Term
--   | BinExpr
--
-- BinExpr = Expr BinOp Expr
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

parseSingleExpr :: TinyParser Expr
parseSingleExpr = parseExpr LowestPrec <* eof

-- VarStmt = "var" Ident "=" Expr ";"
parseVarStmt :: TinyParser Stmt
parseVarStmt = VarStmt
  <$ symbol "var"
  <*> ident
  <* symbol "="
  <*> parseExpr LowestPrec

-- Stmt = VarStmt
parseStmt :: TinyParser Stmt
parseStmt = parseVarStmt

parseStmts :: TinyParser (NonEmptyArray Stmt)
parseStmts = (many1 $ parseStmt <* symbol ";") <* eof
