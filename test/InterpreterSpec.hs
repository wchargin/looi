module InterpreterSpec where

import Test.Hspec

import CoreTypes
import Interpreter

-- STOPSHIP
skip :: Monad m => a -> m ()
skip = const $ return ()

spec :: Spec
spec = do
    it "should evaluate constants" $
        "3" `shouldYield` NumV 3
    skip $ it "should fail on unbound identifiers" $
        "x" `shouldFailWith` "unbound identifier"

    it "should evaluate binary operators successfully" $
        "{+ 1 2}" `shouldYield` NumV 3
    it "should handle division by zero" $
        "{/ 1 0}" `shouldFailWith` "zero"
    it "should handle binary operator type errors" $
        "{<= 3 true}" `shouldFailWith` "num"

    skip $ it "should construct closures" $
        "{func x y {+ x y}}" `shouldYield`
            ClosureV ["x", "y"]
                     (BinopC "+" (IdC "x") (IdC "y"))
                     emptyEnvironment
    skip $ it "should construct closures with a saved environment" $
        "{with {x = 3} {func y x}}" `shouldYield`
            ClosureV ["y"] (IdC "x") (envBind "x" (NumV 3) emptyEnvironment)

    skip $ it "should apply closures" $
        "{{func x y {+ x y}} 3 5}" `shouldYield` NumV 8
    skip $ it "should handle shadowing" $
        "{with {f = {func g x {g {+ 1 x}}}}     \
        \      {g = {func x {+ 1 x}}}           \
        \      {f g 1}}" `shouldYield` NumV 3
    skip $ it "should use lexical scope" $
        "{with {f = {func x}} {g = {func x {f}}} {g 1}}"
            `shouldFailWith` "unbound"
    skip $ it "should handle arity errors" $
        "{{func 1} 2 3}" `shouldFailWith` "arity"

    skip $ it "should handle branching" $
        "{if true 1 2}" `shouldYield` NumV 1
    skip $ it "should short-circuit when branching" $
        "{if false {/ 1 0} 1}" `shouldYield` NumV 1

    skip $ it "should handle Y-like recursive functions" $
        "{with {fact = {func fact                                       \
        \                    {func n                                    \
        \                          {if {<= n 1}                         \
        \                              1                                \
        \                              {* n {{fact fact} {- n 1}}}}}}}  \
        \      {{fact fact} 5}}" `shouldYield` NumV 120

shouldYield :: String -> Value -> Expectation
shouldYield input output = topEval input `shouldBe` Right output

shouldFailWith :: String -> String -> Expectation
shouldFailWith input message = case topEval input of
    Left error      -> error `shouldContain` message
    Right result    -> expectationFailure $ concat
        [ "expected failure matching "
        , show message
        , " but found: "
        , show result
        ]
infix 1 `shouldFailWith`
