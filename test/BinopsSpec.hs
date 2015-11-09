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
