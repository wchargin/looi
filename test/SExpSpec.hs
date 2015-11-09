module SExpSpec(spec) where

import Test.Hspec
import SExp

import Control.Monad

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    parseSexpSpec
    parseQExpRawSpec
    resolveQuasiquoteSpec

parseSexpSpec :: Spec
parseSexpSpec = describe "parseSexp" $ do
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

parseQExpRawSpec :: Spec
parseQExpRawSpec = describe "parseQexpRaw" $ do
    describe "should parse normal s-expressions" $ do
        -- We're a bit light on the testing here
        -- because we assume that the implementation delegates to
        -- the already-tested s-expression parser
        -- (or, rather, that the s-expression parser
        -- is actually based on the quasiquote parser).
        it "like a number" $
            "10" `shouldQParseTo` QNumber 10
        it "like a symbol" $
            "bob" `shouldQParseTo` QSymbol "bob"
        it "like a list" $
            "(1 {two} 33)"
            `shouldQParseTo`
            QList [ QNumber 1
                  , QList [ QSymbol "two" ]
                  , QNumber 33
                  ]

    parseQExpRawUnquoteSpec
    parseQExpRawSpliceSpec

parseQExpRawUnquoteSpec :: Spec
parseQExpRawUnquoteSpec = describe "should parse unquotes" $ do
    it "at the top level" $
        ",x" `shouldQParseTo` QUnquote "x"
    it "at the top level, with a space after the comma" $
        ", y" `shouldQParseTo` QUnquote "y"
    describe "inside a list" $ do
        it "inside a list" $
            "(a ,thing z)"
            `shouldQParseTo`
            QList [ QSymbol "a", QUnquote "thing", QSymbol "z" ]
        it "with a space after the comma" $
            "(a , thing z)"
            `shouldQParseTo`
            QList [ QSymbol "a", QUnquote "thing", QSymbol "z" ]
        it "with multiple consecutively" $
            "(,a ,b ,c)"
            `shouldQParseTo`
            QList (map QUnquote ["a", "b", "c"])
        describe "with no preceding space" $ do
            it "when following a symbol" $
                "(a,b c)"
                `shouldQParseTo`
                QList [ QSymbol "a", QUnquote "b", QSymbol "c" ]
            it "when following a number" $
                "(1,b c)"
                `shouldQParseTo`
                QList [ QNumber 1, QUnquote "b", QSymbol "c" ]
            it "when following another unquote" $
                "(,a,b,c)"
                `shouldQParseTo`
                QList (map QUnquote ["a", "b", "c"])

parseQExpRawSpliceSpec :: Spec
parseQExpRawSpliceSpec = describe "should parse unquote-splicings" $ do
    it "within a list" $ do
        "(thing1 ,@more-things thingN)"
        `shouldQParseTo`
        QList [ QSymbol "thing1"
              , QSplice "more-things"
              , QSymbol "thingN"
              ]
    it "with a space after the \",@\"" $
        ",@ thing" `shouldQParseTo` QSplice "thing"
    it "with multiple consecutively" $
        "(,@a ,@b ,@c)"
        `shouldQParseTo`
        QList (map QSplice ["a", "b", "c"])
    describe "with no preceding space" $ do
        it "when following a symbol" $
            "(a,@b c)"
            `shouldQParseTo`
            QList [ QSymbol "a", QSplice "b", QSymbol "c" ]
        it "when following a number" $
            "(1,@b c)"
            `shouldQParseTo`
            QList [ QNumber 1, QSplice "b", QSymbol "c" ]
        it "when following another unquote-splicing" $
            "(,@a,@b,@c)"
            `shouldQParseTo`
            QList (map QSplice ["a", "b", "c"])
        it "when interspersed with normal unquotes" $
            "(,@a,b,@c,d,@e)"
            `shouldQParseTo`
            QList [ QSplice "a"
                  , QUnquote "b"
                  , QSplice "c"
                  , QUnquote "d"
                  , QSplice "e"
                  ]
    it "even at the top level (will fail during resolution)" $
        ",@thing" `shouldQParseTo` QSplice "thing"
    it "but not with a space between the \",\" and \"@\"" $
        "(, @ thing)"
        `shouldQParseTo`
        QList [ QUnquote "@", QSymbol "thing" ]

resolveQuasiquoteSpec :: Spec
resolveQuasiquoteSpec = describe "resolveQuasiquote" $ do
    let noBindings = ([], [])
    let bindings = ( [ ("a", Number 1) ]
                   , [ ("xs", [ Number 2, Number 3 ])
                     , ("x1", [ Symbol "justOne" ])
                     , ("x0", [])
                     ]
                   )

    it "should expand a single symbol" $
        resolveQuasiquote noBindings (QSymbol "x")
        `shouldBe`
        Right (Symbol "x")
    it "should expand a single number" $
        resolveQuasiquote noBindings (QNumber 20)
        `shouldBe`
        Right (Number 20)
    it "should expand the empty list with no unquotes" $
        resolveQuasiquote noBindings (QList [])
        `shouldBe`
        Right (List [])
    it "should expand a singleton list with no unquotes" $
        resolveQuasiquote noBindings (QList [ QNumber 10 ])
        `shouldBe`
        Right (List [ Number 10 ])
    it "should expand a longer list with no unquotes" $
        resolveQuasiquote noBindings (QList [ QNumber 10, QSymbol "z" ])
        `shouldBe`
        Right (List [ Number 10, Symbol "z" ])

    describe "should expand unquote terms" $ do
        it "at the top level" $ do
            let input = QUnquote "a"
            let result = resolveQuasiquote bindings input
            result `shouldBe` Right (Number 1)
        it "inside a list" $ do
            let input = QList [ QNumber 0, QUnquote "a", QNumber 2 ]
            let result = resolveQuasiquote bindings input
            result `shouldBe` Right (List $ map Number [0, 1, 2])

    describe "when expanding unquote-splicing terms" $ do
        it "should succeed inside a list" $ do
            let input = QList [ QNumber 1, QSplice "xs", QNumber 4 ]
            let result = resolveQuasiquote bindings input
            result `shouldBe` Right (List $ map Number [1, 2, 3, 4])

        -- We include separate tests for splicing terms of different lengths
        -- because one superficially reasonable implementation might just say,
        -- "expand all the terms normally, then make sure we have exactly one;
        -- if we don't, there was a top-level unquote-splicing term,
        -- which is illegal."
        -- This indeed properly handles well-formed quasiquoted expressions,
        -- but does not account for the fact that
        -- a quasiquoted expression at the top level
        -- may contain exactly one term;
        -- this is still an illegal expression and must be handled accordingly.
        describe "should fail at the top level" $ do
            it "with a term of length 0" $ shouldFail $
                resolveQuasiquote bindings (QSplice "x0")
            it "with a term of length 1" $ shouldFail $
                resolveQuasiquote bindings (QSplice "x1")
            it "with a term of length 2" $ shouldFail $
                resolveQuasiquote bindings (QSplice "xs")

    it "should fail on an unbound unquote binding" $
        shouldFail $ resolveQuasiquote noBindings (QUnquote "a")

    it "should fail on an unbound unquote-splicing binding" $
        shouldFail $ resolveQuasiquote noBindings (QList [ QSplice "a" ])

    it "should allow unused unquote and unquote-splicing bindings" $ do
        let input = QList $ map QNumber [10, 11]
        let result = resolveQuasiquote bindings input
        result `shouldBe` Right (List $ map Number [10, 11])

shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail input = input `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft (Right _) = False

shouldParseTo :: String -> SExp -> Expectation
shouldParseTo input output = parseSexp input `shouldBe` Right output

shouldQParseTo :: String -> QExp -> Expectation
shouldQParseTo input output = parseQexpRaw input `shouldBe` Right output

shouldFailToParse :: String -> Expectation
shouldFailToParse = shouldFail . parseSexp

shouldFailToQParse :: String -> Expectation
shouldFailToQParse = shouldFail . parseQexpRaw
