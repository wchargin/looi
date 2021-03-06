module BinopsSpec where

import Test.Hspec

import Control.Monad.Except (Except, runExcept)

import Binops
import CoreTypes
import Data.List (zipWith4)

shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail input = input `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft (Right _) = False

doApplyBinop :: Identifier -> Value -> Value -> Either String Value
doApplyBinop o l r = runExcept $ applyBinop o l r

data NumericBinopSpec = NumericBinopSpec { verb :: String
                                         , opName :: Identifier
                                         , sampleLeft :: NumericValue
                                         , sampleRight :: NumericValue
                                         , sampleResult :: NumericValue
                                         }

numericBinopSpec :: NumericBinopSpec -> Spec
numericBinopSpec (NumericBinopSpec verb op l r res) = do
    let left = NumV l
    let right = NumV r
    let expected = Right $ NumV res
    it ("should " ++ verb ++ " two numeric arguments") $
        doApplyBinop op left right `shouldBe` expected
    it ("should fail to " ++ verb ++ " a non-numeric left operand") $
        shouldFail $ doApplyBinop op (BoolV True) right
    it ("should fail to " ++ verb ++ " a non-numeric right operand") $
        shouldFail $ doApplyBinop op left (BoolV False)
    it ("should fail to " ++ verb ++ " when both operands are non-numeric") $
        shouldFail $ doApplyBinop op (BoolV True) (BoolV False)

spec :: Spec
spec = do
    describe "+" $ numericBinopSpec $
        NumericBinopSpec "add" "+" 1 2 3
    describe "-" $ numericBinopSpec $
        NumericBinopSpec "subtract" "-" 3 5 (-2)
    describe "*" $ numericBinopSpec $
        NumericBinopSpec "multiply" "*" 3 4 12
    describe "/" $ do
        numericBinopSpec $ NumericBinopSpec "divide" "/" 7 3 2
        it "should fail to divide by a non-zero number by zero" $
            shouldFail $ doApplyBinop "/" (NumV 3) (NumV 0)
        it "should fail to divide by zero by zero" $
            shouldFail $ doApplyBinop "/" (NumV 0) (NumV 0)
    describe "eq?" $ do
        let wrap x = Right $ BoolV x
        let eqHuh = doApplyBinop "eq?"
        it "should compare equal numbers" $
            NumV 3 `eqHuh` NumV 3 `shouldBe` wrap True
        it "should compare unequal numbers" $
            NumV 10 `eqHuh` NumV 3 `shouldBe` wrap False
        context "when comparing booleans" $ do
            let test a b c = BoolV a `eqHuh` BoolV b `shouldBe` wrap c
            it "should take (T, T) -> T" $ test True True True
            it "should take (T, F) -> F" $ test True False False
            it "should take (F, T) -> F" $ test False True False
            it "should take (F, F) -> T" $ test False False True
        it "should always compare procedures unequal" $
            let proc = ClosureV ["a", "b"] (IdC "b") emptyEnvironment
            in  proc `eqHuh` proc `shouldBe` wrap False
        it "should compare distinct types unequal" $ do
            NumV 3 `eqHuh` BoolV True `shouldBe` wrap False
            BoolV False `eqHuh` NumV 0 `shouldBe` wrap False
            ClosureV ["a", "b"] (IdC "b") emptyEnvironment
                `eqHuh` NumV 10 `shouldBe` wrap False
    describe "<=" $ do
        let leq = doApplyBinop "<="
        it "should return true when strictly smaller" $
            NumV 3 `leq` NumV 5 `shouldBe` Right (BoolV True)
        it "should return true when equal" $
            NumV 4 `leq` NumV 4 `shouldBe` Right (BoolV True)
        it "should return true when strictly greater" $
            NumV 5 `leq` NumV 3 `shouldBe` Right (BoolV False)
        it "should fail when comparing booleans to numbers" $
            shouldFail $ BoolV True `leq` NumV 3
        it "should fail when comparing booleans to booleans" $
            shouldFail $ BoolV True `leq` BoolV False
        it "should fail when comparing procedures" $
            let proc = ClosureV ["a", "b"] (IdC "b") emptyEnvironment
            in  shouldFail $ proc `leq` proc
