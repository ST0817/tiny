module Test.Tiny.Parsing (spec) where

import Prelude

import Data.Either (Either(..))
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..))
import Tiny.Parsing (Prec(..), parseExpr)

spec :: Spec Unit
spec = describe "parsing" do
  it "integer literal" do
    (runParser "42" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (IntLit 42)

  it "boolean literal" do
    (runParser "true" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BoolLit true)
    (runParser "false" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BoolLit false)

  it "addition" do
    (runParser "42 + 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) AddOp (IntLit 53))

  it "subtraction" do
    (runParser "42 - 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) SubOp (IntLit 53))

  it "multiplication" do
    (runParser "42 * 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) MulOp (IntLit 53))

  it "division" do
    (runParser "42 / 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) DivOp (IntLit 53))

  it "modulo operation" do
    (runParser "42 % 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) ModOp (IntLit 53))

  it "power" do
    (runParser "42 ** 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) PowOp (IntLit 53))

  it "equal" do
    (runParser "42 == 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) EqOp (IntLit 53))

  it "not equal" do
    (runParser "42 != 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) NotEqOp (IntLit 53))

  it "greeter than" do
    (runParser "42 > 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) GTOp (IntLit 53))

  it "less than" do
    (runParser "42 < 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) LTOp (IntLit 53))

  it "greeter than or equal" do
    (runParser "42 >= 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) GEOp (IntLit 53))

  it "less than or equal" do
    (runParser "42 <= 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) LEOp (IntLit 53))

  it "and" do
    (runParser "42 && 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) AndOp (IntLit 53))

  it "or" do
    (runParser "42 || 53" $ parseExpr LowestPrec)
      `shouldEqual`
        Right (BinExpr (IntLit 42) OrOp (IntLit 53))
