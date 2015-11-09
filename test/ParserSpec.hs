module ParserSpec(spec) where

import Test.Hspec

import CoreTypes
import Parser
import SExp

import Control.Monad

spec :: Spec
spec = parseSpec

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "should parse numeric literals" $
        parse (Number 10) `shouldBe` Right (ValueC (NumV 10))
    forM_ [("true", True), ("false", False)] $ \(name, value) ->
        it ("should parse the boolean literal " ++ name) $
            parse (Symbol name) `shouldBe` Right (ValueC (BoolV value))

    context "when parsing identifiers" $ do
        it "should parse other alphanumeric symbols as identifiers" $
            parse (Symbol "bob") `shouldBe` Right (IdC "bob")
        it "should refuse to parse a binary operator as an identifier" $
            parse (Symbol "+") `shouldFailWith` "binary operator"
        it "should refuse to parse a reserved word as an identifier" $
            parse (Symbol "if") `shouldFailWith` "reserved word"

    context "when parsing functions" $ do
        it "should parse a constant function" $
            parse (List [Symbol "func", Number 5]) `shouldBe`
                Right (LambdaC [] $ ValueC (NumV 5))
        it "should parse a unary function" $
            parse (List [Symbol "func", Symbol "x", Symbol "x"]) `shouldBe`
                Right (LambdaC ["x"] $ IdC "x")
        it "should parse a ternary function" $
            parse (List [ Symbol "func"
                        , Symbol "x"
                        , Symbol "y"
                        , Symbol "z"
                        , Symbol "y"
                        ]) `shouldBe`
                Right (LambdaC ["x", "y", "z"] $ IdC "y")
        it "should parse a nested function definition" $
            parse (List [ Symbol "func"
                        , Symbol "x"
                        , List [ Symbol "func", Symbol "x" ]]) `shouldBe`
                Right (LambdaC ["x"] $ LambdaC [] $ IdC "x")
        it "should fail to parse an empty function definition" $
            parse (List [Symbol "func"]) `shouldFailWith` "body"
        it "should fail to parse a function with a non-symbol parameter" $
            parse (List [Symbol "func", Number 5, Number 10])
                `shouldFailWith` "symbol"
        it "should fail to parse a function with an illegal parameter name" $
            parse (List [Symbol "func", Symbol "if", Symbol "bad"])
                `shouldFailWith` "illegal identifier"

    it "should fail on an empty application" $
        parse (List []) `shouldFailWith` "empty application"

    context "when parsing binary operators" $ do
        it "should accept \"+\" with two operands" $
            parse (List [Symbol "+", Number 1, Number 2]) `shouldBe`
                Right (BinopC "+" (ValueC (NumV 1)) (ValueC (NumV 2)))
        forM_ (zip [0, 1, 3]
                   ["no operands", "one operand", "three operands"]) $
            \(count, name) -> it ("should reject \"+\" with " ++ name) $
                parse (List $ Symbol "+" : map Number [1..count])
                    `shouldFailWith` "arity"

    context "when parsing applications" $ do
        it "should parse a nullary function application" $
            parse (List [Symbol "const5"]) `shouldBe`
                Right (AppC (IdC "const5") [])
        it "should parse a unary function application" $
            parse (List [Symbol "add1", Number 5]) `shouldBe`
                Right (AppC (IdC "add1") [ValueC $ NumV 5])
        it "should parse a binary function application" $
            parse (List [Symbol "expt", Number 5, Number 10]) `shouldBe`
                Right (AppC (IdC "expt") $ map (ValueC . NumV) [5, 10])
        it "should parse a ternary function application" $
            parse (List [ Symbol "my-if"
                        , Symbol "true"
                        , Number 10
                        , Symbol "other"
                        ]) `shouldBe`
                Right (AppC (IdC "my-if")
                       [ValueC (BoolV True), ValueC (NumV 10), IdC "other"])


shouldFailWith :: (Show b, Eq b) => Either String b -> String -> Expectation
shouldFailWith (Left msg) test = msg `shouldContain` test
shouldFailWith (Right result) msg = expectationFailure $ concat
    [ "expected failure matching "
    , show msg
    , " but found: "
    , show result
    ]
infix 1 `shouldFailWith`
