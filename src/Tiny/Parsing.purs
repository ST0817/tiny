module Tiny.Parsing
  ( parsePattern
  , parseSingleExpr
  , parseStmt
  , parseStmts
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Lazy (defer)
import Data.Array (foldl, fromFoldable, singleton)
import Data.Tuple.Nested ((/\))
import Parsing (Parser)
import Parsing.Combinators (optionMaybe, try)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.String (eof)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (BinOp(..), Expr(..), Pattern(..), Stmt(..))

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

parens :: forall a. TinyParser a -> TinyParser a
parens = tokenParser.parens

commaSep :: forall a. TinyParser a -> TinyParser (Array a)
commaSep parser = fromFoldable <$> tokenParser.commaSep parser

-- VarPattern = Ident
parseVarPattern :: TinyParser Pattern
parseVarPattern = VarPattern <$> ident

-- TuplePattern = "(" Pattern ( "," Pattern )* ")"
--
-- Parenthesized patterns are parsed as tuple patterns with a single element.
-- The element is evaluated as a single pattern during evaluation.
parseTuplePattern :: TinyParser Pattern
parseTuplePattern = defer \_ ->
  TuplePattern <$> parens (commaSep parsePattern)

-- Pattern
--   = VarPattern
--   | TuplePattern
parsePattern :: TinyParser Pattern
parsePattern = defer \_ ->
  try parseVarPattern
    <|> try parseTuplePattern

-- FloatLit = [0-9]+( "." [0-9]+ )?
parseFloatLit :: TinyParser Expr
parseFloatLit = FloatLit <$> tokenParser.float

-- IntLit = [0-9]+
parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer

-- BoolLit = "true" | "false"
parseBoolLit :: TinyParser Expr
parseBoolLit =
  symbol "true" $> BoolLit true
    <|> symbol "false" $> BoolLit false

-- NullLit = "null"
parseNullLit :: TinyParser Expr
parseNullLit = NullLit <$ symbol "null"

-- Var = Ident
parseVar :: TinyParser Expr
parseVar = Var <$> ident

-- TupleExpr = "(" Expr ( "," Expr )* ")"
--
-- Parenthesized expressions are parsed as tuples with a single element.
-- The element is evaluated as a single expression during evaluation.
parseTupleExpr :: TinyParser Expr
parseTupleExpr = defer \_ ->
  TupleExpr <$> parens (commaSep $ parseExpr LowestPrec)

-- Term
--   = FloatLit
--   | IntLit
--   | BoolLit
--   | NullLit
--   | Var
--   | TupleExpr
parseTerm :: TinyParser Expr
parseTerm = defer \_ ->
  try parseFloatLit
    <|> try parseIntLit
    <|> try parseBoolLit
    <|> try parseNullLit
    <|> try parseVar
    <|> parseTupleExpr

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

-- VarStmt = "var" Pattern ( "=" Expr )? ";"
-- "var foo;" is desugared to "var foo = null;"
parseVarStmt :: TinyParser Stmt
parseVarStmt = do
  _ <- symbol "var"
  pattern <- parsePattern
  VarStmt pattern
    <$> value pattern
    <* semi
  where
  defaultValue pattern = case pattern of
    VarPattern _ -> NullLit
    TuplePattern patterns -> TupleExpr $ defaultValue <$> patterns

  value pattern =
    symbol "=" *> parseExpr LowestPrec <|> pure (defaultValue pattern)

-- AssignStmt = Pattern "=" Expr ";"
parseAssignStmt :: TinyParser Stmt
parseAssignStmt = AssignStmt
  <$> parsePattern
  <* symbol "="
  <*> parseExpr LowestPrec
  <* semi

block :: TinyParser (Array Stmt)
block = defer \_ -> tokenParser.braces $ many parseStmt

-- IfStmt = "if" Expr "{" Stmt* "}" ( "else" ( "{" Stmt* "}" ) | IfStmt )?
--
-- "... } else if ..." is desugared to "... } else { if ...".
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
