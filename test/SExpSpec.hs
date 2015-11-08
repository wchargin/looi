module SExpSpec(spec) where

import Test.Hspec
import SExp

import Control.Monad

shouldParseTo :: String -> SExp -> Expectation
shouldParseTo input output = parse input `shouldBe` Right output

shouldFailToParse :: String -> Expectation
shouldFailToParse input = parse input `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft (Right _) = False

-- for brevity
parse = parseSexp

specSymbols :: Spec
specSymbols = do
    it "should parse alphabetic symbols" $
        "abc" `shouldParseTo` Symbol "abc"
    it "should parse alphanumeric symbols" $
        "a113" `shouldParseTo` Symbol "a113"
    it "should parse symbols with alphanumeric and special characters" $
        "secret:a-113" `shouldParseTo` Symbol "secret:a-113"
    it "should parse non-ASCII characters" $
        "β-carotene" `shouldParseTo` Symbol "β-carotene"
    it "should reject symbols with initial digits" $
        shouldFailToParse "113-a"

specNumbers :: Spec
specNumbers = do
    it "should parse positive numbers" $
        "123" `shouldParseTo` Number 123
    it "should parse zero" $
        "0" `shouldParseTo` Number 0
    it "should parse negative numbers" $
        "-123" `shouldParseTo` Number (-123)

specLists :: Spec
specLists = do
    forM_ [ ("parentheses", "(1 2 3)")
          , ("brackets", "[1 2 3]")
          , ("braces", "{1 2 3}")
          ] $
          \(kind, input) ->
              it ("should parse lists with " ++ kind) $
              input `shouldParseTo` (List $ map Number [1..3])
    it "should parse heterogeneous lists" $
        "(hal 9000)" `shouldParseTo` List [ Symbol "hal", Number 9000 ]
    it "should parse nested lists with the same delimiter" $
        "(1 2 (3 4))" `shouldParseTo`
        List [ Number 1, Number 2, List [ Number 3, Number 4 ] ]
    it "should parse nested lists with different delimiters" $
        "(1 2 [3 4])" `shouldParseTo`
        List [ Number 1, Number 2, List [ Number 3, Number 4 ] ]
    it "should reject nested lists with mismatched delimiters" $
        shouldFailToParse "(1 2 [3 4)]"
    it "should parse the empty list" $
        "()" `shouldParseTo` List []
    it "should parse the empty list with spaces" $
        "{  }" `shouldParseTo` List []
    it "should reject an unterminated list" $
        shouldFailToParse "("
    it "should reject a closing delimiter with no open delimiter" $
        shouldFailToParse ")"

specSpaces :: Spec
specSpaces = do
    it "should parse a list with trailing internal spaces" $
        "(1 )" `shouldParseTo` List [Number 1]
    it "should parse a list with leading internal spaces" $
        "( 1)" `shouldParseTo` List [Number 1]
    it "should parse a list with both leading and trailing internal spaces" $
        "( 1 )" `shouldParseTo` List [Number 1]
    it "should parse a list with both leading and trailing internal spaces" $
        "  ( 1 )  " `shouldParseTo` List [Number 1]
    it "should parse a symbol with both leading and trailing spaces" $
        "  abc  " `shouldParseTo` Symbol "abc"
    it "should parse a number with both leading and trailing spaces" $
        "  37  " `shouldParseTo` Number 37

spec :: Spec
spec =
    describe "parseSexp" $ do
        describe "for symbols" specSymbols
        describe "for numbers" specNumbers
        describe "for lists" specLists
        describe "regarding spaces" specSpaces
        it "should parse a somewhat complicated expression" $
            "{abc 123 -345 (b {} 12) zyxwvut}" `shouldParseTo`
            List [ Symbol "abc"
                 , Number 123
                 , Number (-345)
                 , List [ Symbol "b"
                        , List []
                        , Number 12
                        ]
                 , Symbol "zyxwvut"
                 ]
