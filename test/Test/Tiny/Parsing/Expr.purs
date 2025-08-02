module Test.Tiny.Parsing.Expr (spec) where

import Prelude

import Data.Either (Either(..))
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tiny.Ast (BinOp(..), Expr(..))
import Tiny.Parsing (parseSingleExpr)

spec :: Spec Unit
spec = describe "expression" do
  it "floating point number literal" do
    shouldEqual
      (runParser "42.195" parseSingleExpr)
      (Right (FloatLit 42.195))

  it "integer literal" do
    shouldEqual
      (runParser "42" parseSingleExpr)
      (Right (IntLit 42))

  it "boolean literal" do
    shouldEqual
      (runParser "true" parseSingleExpr)
      (Right $ BoolLit true)
    shouldEqual
      (runParser "false" parseSingleExpr)
      (Right $ BoolLit false)

  it "null literal" do
    shouldEqual
      (runParser "null" parseSingleExpr)
      (Right NullLit)

  it "variable" do
    shouldEqual
      (runParser "foo" parseSingleExpr)
      (Right $ Var "foo")

  it "parenthesized expression" do
    let
      lhs = TupleExpr [ BinExpr (IntLit 42) AddOp (IntLit 13) ]
      rhs = IntLit 53
    shouldEqual
      (runParser "(42 + 13) * 53" parseSingleExpr)
      (Right $ BinExpr lhs MulOp rhs)

  it "tuple" do
    shouldEqual
      (runParser "(42, true)" parseSingleExpr)
      (Right $ TupleExpr [ IntLit 42, BoolLit true ])

  it "unit" do
    shouldEqual
      (runParser "()" parseSingleExpr)
      (Right $ TupleExpr [])

  it "addition" do
    shouldEqual
      (runParser "42 + 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) AddOp (IntLit 53))

  it "subtraction" do
    shouldEqual
      (runParser "42 - 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) SubOp (IntLit 53))

  it "multiplication" do
    shouldEqual
      (runParser "42 * 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) MulOp (IntLit 53))

  it "division" do
    shouldEqual
      (runParser "42 / 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) DivOp (IntLit 53))

  it "modulo operation" do
    shouldEqual
      (runParser "42 % 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) ModOp (IntLit 53))

  it "power (integer)" do
    shouldEqual
      (runParser "42 ** 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) PowOp (IntLit 53))

  it "power (floating point number)" do
    shouldEqual
      (runParser "36.0 ** 0.5" parseSingleExpr)
      (Right $ BinExpr (FloatLit 36.0) PowOp (FloatLit 0.5))

  it "equal" do
    shouldEqual
      (runParser "42 == 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) EqOp (IntLit 53))

  it "not equal" do
    shouldEqual
      (runParser "42 != 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) NotEqOp (IntLit 53))

  it "greeter than" do
    shouldEqual
      (runParser "42 > 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) GTOp (IntLit 53))

  it "less than" do
    shouldEqual
      (runParser "42 < 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) LTOp (IntLit 53))

  it "greeter than or equal" do
    shouldEqual
      (runParser "42 >= 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) GEOp (IntLit 53))

  it "less than or equal" do
    shouldEqual
      (runParser "42 <= 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) LEOp (IntLit 53))

  it "and" do
    shouldEqual
      (runParser "42 && 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) AndOp (IntLit 53))

  it "or" do
    shouldEqual
      (runParser "42 || 53" parseSingleExpr)
      (Right $ BinExpr (IntLit 42) OrOp (IntLit 53))
