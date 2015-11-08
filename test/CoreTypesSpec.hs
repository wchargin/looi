module CoreTypesSpec where

import Test.Hspec

import CoreTypes

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "Environment" $ do
        let env = foldr (uncurry envBind) emptyEnvironment
                  [("x", NumV 10), ("y", NumV 20)]
        it "looks up bound values" $ do
            envLookup "x" env `shouldBe` Just (NumV 10)
            envLookup "y" env `shouldBe` Just (NumV 20)
        it "looks up unbound values" $ do
            envLookup "z" env `shouldBe` Nothing
