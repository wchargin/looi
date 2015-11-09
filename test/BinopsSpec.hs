module BinopsSpec where

import Test.Hspec

import Binops
import CoreTypes
import Data.List (zipWith4)

shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail input = input `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft (Right _) = False

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
        applyBinop op left right `shouldBe` expected
    it ("should fail to " ++ verb ++ " a non-numeric left operand") $
        shouldFail $ applyBinop op (BoolV True) right
    it ("should fail to " ++ verb ++ " a non-numeric right operand") $
        shouldFail $ applyBinop op left (BoolV False)
    it ("should fail to " ++ verb ++ " when both operands are non-numeric") $
        shouldFail $ applyBinop op (BoolV True) (BoolV False)

spec :: Spec
spec = do
    describe "addition" $ numericBinopSpec $
        NumericBinopSpec "add" "+" 1 2 3
    describe "subtraction" $ numericBinopSpec $
        NumericBinopSpec "subtract" "-" 3 5 (-2)
    describe "multiplication" $ numericBinopSpec $
        NumericBinopSpec "multiply" "*" 3 4 12
    describe "division" $ do
        numericBinopSpec $ NumericBinopSpec "divide" "/" 7 3 2
        it "should fail to divide by a non-zero number by zero" $
            shouldFail $ applyBinop "/" (NumV 3) (NumV 0)
        it "should fail to divide by zero by zero" $
            shouldFail $ applyBinop "/" (NumV 0) (NumV 0)
    describe "eq?" $ do
        let wrap x = Right $ BoolV x
        let eqHuh = applyBinop "eq?"
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
            in  applyBinop "eq?" proc proc `shouldBe` wrap False
    describe "<=" $ do
        let leq = applyBinop "<="
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
