module SExpSpec(spec) where

import Test.Hspec
import SExp

import Control.Monad

shouldYield :: (Eq a, Eq b, Show a, Show b) => Either a b -> b -> Expectation
shouldYield input output = input `shouldBe` Right output

shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail input = input `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft (Right _) = False

-- for brevity
parse = parseSexp

specSymbols :: Spec
specSymbols = do
    it "should parse alphabetic symbols" $
        parse "abc" `shouldYield` Symbol "abc"
    it "should parse alphanumeric symbols" $
        parse "a113" `shouldYield` Symbol "a113"
    it "should parse symbols with alphanumeric and special characters" $
        parse "secret:a-113" `shouldYield` Symbol "secret:a-113"
    it "should parse non-ASCII characters" $
        parse "β-carotene" `shouldYield` Symbol "β-carotene"
    it "should fail on symbols with initial digits" $
        shouldFail $ parse "113-a"

specNumbers :: Spec
specNumbers = do
    it "should parse positive numbers" $
        parse "123" `shouldYield` Number 123
    it "should parse zero" $
        parse "0" `shouldYield` Number 0
    it "should parse negative numbers" $
        parse "-123" `shouldYield` Number (-123)

specLists :: Spec
specLists = do
    forM_ [ ("parentheses", "(1 2 3)")
          , ("brackets", "[1 2 3]")
          , ("braces", "{1 2 3}")
          ] $
          \(kind, input) ->
              it ("should parse lists with " ++ kind) $
              parse input `shouldYield` (List $ map Number [1..3])
    it "should parse heterogeneous lists" $
        parse "(hal 9000)" `shouldYield` List [ Symbol "hal", Number 9000 ]
    it "should parse nested lists with the same delimiter" $
        parse "(1 2 (3 4))" `shouldYield`
        List [ Number 1, Number 2, List [ Number 3, Number 4 ] ]
    it "should parse nested lists with different delimiters" $
        parse "(1 2 [3 4])" `shouldYield`
        List [ Number 1, Number 2, List [ Number 3, Number 4 ] ]
    it "should fail on nested lists with mismatched delimiters" $
        shouldFail $ parse "(1 2 [3 4)]"
    it "should parse the empty list" $
        parse "()" `shouldYield` List []
    it "should parse the empty list with spaces" $
        parse "{  }" `shouldYield` List []

specSpaces :: Spec
specSpaces = do
    it "should parse a list with trailing internal spaces" $
        parse "(1 )" `shouldYield` List [Number 1]
    it "should parse a list with leading internal spaces" $
        parse "( 1)" `shouldYield` List [Number 1]
    it "should parse a list with both leading and trailing internal spaces" $
        parse "( 1 )" `shouldYield` List [Number 1]
    it "should parse a list with both leading and trailing internal spaces" $
        parse "  ( 1 )  " `shouldYield` List [Number 1]
    it "should parse a symbol with both leading and trailing spaces" $
        parse "  abc  " `shouldYield` Symbol "abc"
    it "should parse a number with both leading and trailing spaces" $
        parse "  37  " `shouldYield` Number 37

spec :: Spec
spec =
    describe "parseSexp" $ do
        describe "for symbols" specSymbols
        describe "for numbers" specNumbers
        describe "for lists" specLists
        describe "regarding spaces" specSpaces
        it "should parse a somewhat complicated expression" $
            parse "{abc 123 -345 (b {} 12) zyxwvut}" `shouldYield`
            List [ Symbol "abc"
                 , Number 123
                 , Number (-345)
                 , List [ Symbol "b"
                        , List []
                        , Number 12
                        ]
                 , Symbol "zyxwvut"
                 ]
