module Tiny.Parsing where

import Prelude

import Parsing (Parser)
import Parsing.Language (emptyDef)
import Parsing.Token (TokenParser, makeTokenParser)
import Tiny.Ast (Expr(..))

type TinyParser = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

parseIntLit :: TinyParser Expr
parseIntLit = IntLit <$> tokenParser.integer
