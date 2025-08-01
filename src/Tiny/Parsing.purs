module Tiny.Parsing (parseSingleExpr, parseStmt, parseStmts) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Lazy (defer)
import Data.Array (foldl, singleton)
import Data.Tuple.Nested ((/\))
import Parsing (Parser)
import Parsing.Combinators (optionMaybe, try)
import Parsing.Combinators.Array (many)
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

-- NullLit = "null"
parseNullLit :: TinyParser Expr
parseNullLit = NullLit <$ symbol "null"

-- Term
--   = IntLit
--   | BoolLit
--   | NullLit
--   | Var
--   | "(" Expr ")"
parseTerm :: TinyParser Expr
parseTerm = defer \_ ->
  parseIntLit
    <|> parseBoolLit
    <|> parseNullLit
    <|> parseVar
    <|> tokenParser.parens (parseExpr LowestPrec)

-- Expr
--   = Term
--   | BinExpr
--
-- BinExpr = Expr BinOp Expr
--
-- BinOp
--   = "+" | "-" | "*" | "/" | "%" | "**"
--   | "==" | "!=" | ">" | "<" | ">=" | "<="
--   | "&&" | "||"
parseExpr :: Prec -> TinyParser Expr
parseExpr prec = do
  first <- parseTerm
  rest <- many $ try do
    (op /\ opPrec /\ isRightAssoc) <-
      symbol "**" $> PowOp /\ ProdPrec /\ true
        <|> symbol "==" $> EqOp /\ CompPrec /\ false
        <|> symbol "!=" $> NotEqOp /\ CompPrec /\ false
        <|> symbol ">=" $> GEOp /\ CompPrec /\ false
        <|> symbol "<=" $> LEOp /\ CompPrec /\ false
        <|> symbol "&&" $> AndOp /\ AndPrec /\ false
        <|> symbol "||" $> OrOp /\ OrPrec /\ false
        <|> symbol "+" $> AddOp /\ SumPrec /\ false
        <|> symbol "-" $> SubOp /\ SumPrec /\ false
        <|> symbol "*" $> MulOp /\ ProdPrec /\ false
        <|> symbol "/" $> DivOp /\ ProdPrec /\ false
        <|> symbol "%" $> ModOp /\ ProdPrec /\ false
        <|> symbol ">" $> GTOp /\ CompPrec /\ false
        <|> symbol "<" $> LTOp /\ CompPrec /\ false
    guard $ opPrec > prec || isRightAssoc
    rhs <- parseExpr opPrec
    pure (op /\ rhs)
  pure $ foldl (\lhs (op /\ rhs) -> BinExpr lhs op rhs) first rest

parseSingleExpr :: TinyParser Expr
parseSingleExpr = parseExpr LowestPrec <* eof

semi :: TinyParser String
semi = tokenParser.semi

-- VarStmt = "var" Ident ( "=" Expr )? ";"
-- "var foo;" is desugared to "var foo = null;"
parseVarStmt :: TinyParser Stmt
parseVarStmt = VarStmt
  <$ symbol "var"
  <*> ident
  <*> (symbol "=" *> parseExpr LowestPrec <|> pure NullLit)
  <* semi

-- AssignStmt = Ident "=" Expr ";"
parseAssignStmt :: TinyParser Stmt
parseAssignStmt = AssignStmt
  <$> ident
  <* symbol "="
  <*> parseExpr LowestPrec
  <* semi

block :: TinyParser (Array Stmt)
block = defer \_ -> tokenParser.braces $ many parseStmt

-- IfStmt = "if" Expr "{" Stmt* "}" ( "else" ( "{" Stmt* "}" ) | IfStmt )?
-- "... } else if ..." is desugared to "... } else { if ..."
parseIfStmt :: TinyParser Stmt
parseIfStmt = defer \_ -> IfStmt
  <$ symbol "if"
  <*> parseExpr LowestPrec
  <*> block
  <*> optionMaybe
    (symbol "else" *> (singleton <$> parseIfStmt <|> block))

-- Stmt
--   = VarStmt
--   | AssignStmt
--   | IfStmt
parseStmt :: TinyParser Stmt
parseStmt = defer \_ ->
  try parseVarStmt
    <|> try parseAssignStmt
    <|> parseIfStmt

parseStmts :: TinyParser (Array Stmt)
parseStmts = tokenParser.whiteSpace
  *> (many parseStmt)
  <* eof
