module ParserSpec(spec) where

import Test.Hspec

import CoreTypes
import Parser
import SExp

import Control.Monad
import Control.Monad.Except (runExcept)

spec :: Spec
spec = do
    parseSpec
    topParseSpec

doParse :: SExp -> Either String ExprC
doParse = runExcept . parse

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "should parse numeric literals" $
        doParse (Number 10)
            `shouldBe` Right (ValueC (NumV 10))
    forM_ [("true", True), ("false", False)] $ \(name, value) ->
        it ("should parse the boolean literal " ++ name) $
            doParse (Symbol name)
                `shouldBe` Right (ValueC (BoolV value))

    context "when parsing identifiers" $ do
        it "should parse other alphanumeric symbols as identifiers" $
            doParse (Symbol "bob")
                `shouldBe` Right (IdC "bob")
        it "should refuse to parse a binary operator as an identifier" $
            doParse (Symbol "+")
                `shouldFailWith` "binary operator"
        it "should refuse to parse a reserved word as an identifier" $
            doParse (Symbol "if")
                `shouldFailWith` "reserved word"

    it "should fail on strings" $
        doParse (String "\"hi\"")
            `shouldFailWith` "string"

    context "when parsing functions" $ do
        it "should parse a constant function" $
            doParse (List [Symbol "func", Number 5])
                `shouldBe` Right (LambdaC [] $ ValueC (NumV 5))
        it "should parse a unary function" $
            doParse (List [Symbol "func", Symbol "x", Symbol "x"])
                `shouldBe` Right (LambdaC ["x"] $ IdC "x")
        it "should parse a ternary function" $
            doParse (List [ Symbol "func"
                        , Symbol "x"
                        , Symbol "y"
                        , Symbol "z"
                        , Symbol "y"
                        ])
                `shouldBe` Right (LambdaC ["x", "y", "z"] $ IdC "y")
        it "should parse a nested function definition" $
            doParse (List [ Symbol "func"
                        , Symbol "x"
                        , List [ Symbol "func", Symbol "x" ]])
                `shouldBe` Right (LambdaC ["x"] $ LambdaC [] $ IdC "x")
        it "should fail to parse an empty function definition" $
            doParse (List [Symbol "func"])
                `shouldFailWith` "body"
        it "should fail to parse a function with a non-symbol parameter" $
            doParse (List [Symbol "func", Number 5, Number 10])
                `shouldFailWith` "symbol"
        it "should fail to parse a function with an illegal parameter name" $
            doParse (List [Symbol "func", Symbol "if", Symbol "bad"])
                `shouldFailWith` "illegal identifier"

    it "should fail on an empty application" $
        doParse (List []) `shouldFailWith` "empty application"

    context "when parsing binary operators" $ do
        it "should accept \"+\" with two operands" $
            doParse (List [Symbol "+", Number 1, Number 2])
                `shouldBe`
                Right (BinopC "+" (ValueC (NumV 1)) (ValueC (NumV 2)))
        forM_ (zip [0, 1, 3]
                   ["no operands", "one operand", "three operands"]) $
            \(count, name) -> it ("should reject \"+\" with " ++ name) $
                doParse (List $ Symbol "+" : map Number [1..count])
                    `shouldFailWith` "arity"

    context "when parsing a `with'-statement" $ do
        it "should fail on an empty `with'-statement" $
            doParse (List [Symbol "with"]) `shouldFailWith` "empty"
        it "should fail on a malformed clause" $
            doParse (List [ Symbol "with"
                        , List [ Symbol "x", Number 10 ]
                        , Symbol "x"
                        ])
                `shouldFailWith` "malformed"
        it "should accept a `with'-statement with a body but no bindings" $
            doParse (List [Symbol "with", Number 10])
                `shouldBe` Right (AppC (LambdaC [] (ValueC (NumV 10))) [])
        it "should accept a `with'-statement with one binding" $
            doParse (List [ Symbol "with"
                        , List [ Symbol "x", Symbol "=", Number 10 ]
                        , List [ Symbol "+", Symbol "x", Number 1 ]
                        ]) `shouldBe`
                Right (AppC (LambdaC ["x"]
                                     (BinopC "+" (IdC "x") (ValueC (NumV 1))))
                            [ValueC (NumV 10)])
        it "should accept a `with'-statement with two bindings" $
            doParse (List [ Symbol "with"
                        , List [ Symbol "x", Symbol "=", Number 10 ]
                        , List [ Symbol "y", Symbol "=", Number 20]
                        , List [ Symbol "+", Symbol "x", Symbol "y" ]
                        ]) `shouldBe`
                Right (AppC (LambdaC ["x", "y"]
                                     (BinopC "+" (IdC "x") (IdC "y")))
                            [ValueC (NumV 10), ValueC (NumV 20)])

    context "when parsing conditionals" $ do
        it "should parse a valid if-expression" $
            doParse
                (List [Symbol "if", Symbol "bool", Number 10, Number 20])
            `shouldBe`
            Right (IfC (IdC "bool") (ValueC (NumV 10)) (ValueC (NumV 20)))
        forM_ (zip [0, 1, 2, 4]
                   [ "no operands"
                   , "one operand"
                   , "two operands"
                   , "four operands"]) $
            \(count, name) ->
                it ("should reject an if-expression with " ++ name) $
                    doParse
                        (List $ Symbol "if" : map Number [1..count])
                        `shouldFailWith` "arity"

    context "when parsing applications" $ do
        it "should parse a nullary function application" $
            doParse (List [Symbol "const5"])
            `shouldBe` Right (AppC (IdC "const5") [])
        it "should parse a unary function application" $
            doParse (List [Symbol "add1", Number 5])
            `shouldBe` Right (AppC (IdC "add1") [ValueC $ NumV 5])
        it "should parse a binary function application" $
            doParse (List [Symbol "expt", Number 5, Number 10])
            `shouldBe` Right (AppC (IdC "expt") $ map (ValueC . NumV) [5, 10])
        it "should parse a ternary function application" $
            doParse (List [ Symbol "my-if"
                        , Symbol "true"
                        , Number 10
                        , Symbol "other"
                        ])
                `shouldBe`
                Right (AppC (IdC "my-if")
                       [ValueC (BoolV True), ValueC (NumV 10), IdC "other"])

    context "when parsing new-arrays" $ do
        it "should parse a new-array with literals" $
            doParse (List [ Symbol "new-array"
                          , Number 10
                          , Symbol "true"
                          ]) `shouldBe`
                Right (NewArrayC (ValueC $ NumV 10) (ValueC $ BoolV True))
        it "should parse a new-array with expression opearnds" $
            doParse (List [ Symbol "new-array"
                          , List [Symbol "+", Number 1, Number 2]
                          , List [Symbol "+", Number 3, Number 4]
                          ]) `shouldBe`
            Right (NewArrayC (BinopC "+" (ValueC $ NumV 1) (ValueC $ NumV 2))
                            (BinopC "+" (ValueC $ NumV 3) (ValueC $ NumV 4)))

topParseSpec :: Spec
topParseSpec = describe "topParse" $ do
    it "should properly parse a reasonable program" $
        (runExcept . topParse) "((func any 5) (* x y))"
        `shouldBe`
            Right (AppC (LambdaC ["any"] (ValueC $ NumV 5))
                        [BinopC "*" (IdC "x") (IdC "y")])
    it "should reject programs with quasiquotation" $
        (runExcept . topParse) "(add 1 ,x)"
            `shouldFailWith` "quasiquotation is not allowed"

shouldFailWith :: (Show b, Eq b) => Either String b -> String -> Expectation
shouldFailWith (Left msg) test = msg `shouldContain` test
shouldFailWith (Right result) msg = expectationFailure $ concat
    [ "expected failure matching "
    , show msg
    , " but found: "
    , show result
    ]
infix 1 `shouldFailWith`
