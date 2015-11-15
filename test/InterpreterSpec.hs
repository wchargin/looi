{-# LANGUAGE TupleSections #-}

module InterpreterSpec where

import Test.Hspec

import qualified Data.Map as Map
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)

import CoreTypes
import Interpreter

spec :: Spec
spec = do
    it "should evaluate constants" $
        "3" `shouldYield` NumV 3
    it "should fail on unbound identifiers" $
        "x" `shouldFailWith` "unbound identifier"

    it "should evaluate binary operators successfully" $
        "{+ 1 2}" `shouldYield` NumV 3
    it "should handle division by zero" $
        "{/ 1 0}" `shouldFailWith` "zero"
    it "should handle binary operator type errors" $
        "{<= 3 true}" `shouldFailWith` "num"

    it "should construct arrays" $
        (runExcept . parseEval) "{new-array 3 5}" `shouldBe`
            Right (ArrayV 0 3,
                   Store (Map.fromList $ map (, NumV 5) [0, 1, 2]) 3)
    it "should reject arrays with a non-numeric length" $
        "{new-array true 10}" `shouldFailWith` "numeric"

    it "should construct closures" $
        "{func x y {+ x y}}" `shouldYield`
            ClosureV ["x", "y"]
                     (BinopC "+" (IdC "x") (IdC "y"))
                     emptyEnvironment
    it "should construct closures with a saved environment" $
        "{with {x = 3} {func y x}}" `shouldYield`
            ClosureV ["y"] (IdC "x") (envBind "x" 0 emptyEnvironment)

    it "should apply closures" $
        "{{func x y {+ x y}} 3 5}" `shouldYield` NumV 8
    it "should handle shadowing" $
        "{with {f = {func g x {g {+ 1 x}}}}     \
        \      {g = {func x {+ 1 x}}}           \
        \      {f g 1}}" `shouldYield` NumV 3
    it "should use lexical scope" $
        "{with {f = {func x}} {g = {func x {f}}} {g 1}}"
            `shouldFailWith` "unbound"
    it "should handle arity errors" $
        "{{func 1} 2 3}" `shouldFailWith` "arity"

    it "should handle branching" $
        "{if true 1 2}" `shouldYield` NumV 1
    it "should short-circuit when branching" $
        "{if false {/ 1 0} 1}" `shouldYield` NumV 1

    it "should handle Y-like recursive functions" $
        "{with {fact = {func fact                                       \
        \                    {func n                                    \
        \                          {if {<= n 1}                         \
        \                              1                                \
        \                              {* n {{fact fact} {- n 1}}}}}}}  \
        \      {{fact fact} 5}}" `shouldYield` NumV 120

shouldYield :: String -> Value -> Expectation
shouldYield input output = runExcept (topEval input) `shouldBe` Right output

shouldFailWith :: String -> String -> Expectation
shouldFailWith input message = case runExcept (topEval input) of
    Left error      -> error `shouldContain` message
    Right result    -> expectationFailure $ concat
        [ "expected failure matching "
        , show message
        , " but found: "
        , show result
        ]
infix 1 `shouldFailWith`
