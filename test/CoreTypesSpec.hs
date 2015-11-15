module CoreTypesSpec where

import Test.Hspec

import Control.Monad (forM)
import Control.Monad.State (execState, runState)

import CoreTypes

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "Environment" $ do
        let env = foldr (uncurry envBind) emptyEnvironment
                  [("x", 10), ("y", 20)]
        it "looks up bound values" $ do
            envLookup "x" env `shouldBe` Just 10
            envLookup "y" env `shouldBe` Just 20
        it "looks up unbound values" $ do
            envLookup "z" env `shouldBe` Nothing

    describe "Store" $ do
        let st = let vals = [NumV 10, BoolV False, NumV 20]
                 in  flip execState emptyStore $ forM vals allocate
        it "looks up stored values" $ do
            runState (storeLookup 0) st `shouldBe` (Just $ NumV 10, st)
            runState (storeLookup 1) st `shouldBe` (Just $ BoolV False, st)
            runState (storeLookup 2) st `shouldBe` (Just $ NumV 20, st)
        it "looks up non-stored values" $ do
            runState (storeLookup 3) st `shouldBe` (Nothing, st)

    describe "serialize" $ do
        it "should serialize numeric values" $
            serialize (NumV 12) `shouldBe` "12"
        it "should serialize boolean values" $
            serialize (BoolV True) `shouldBe` "true"
        it "should serialize array values, losing their contents" $
            serialize (ArrayV 3 5) `shouldBe` "#<array>"
        it "should serialize closures, losing all the information" $
            serialize (ClosureV ["x", "y"]
                                (IdC "x")
                                (envBind "z" 10 emptyEnvironment))
                `shouldBe` "#<procedure>"
