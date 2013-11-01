module Data.Util.MaybeSpec ( spec ) where

import Test.Hspec
import Data.Util.Maybe

spec :: Spec
spec = describe "Maybe" $ do
    describe "fromMaybe" $ do
        it "uses the default value if Nothing" $
            fromMaybe 3 Nothing `shouldBe` 3
        it "uses value if Just value" $
            fromMaybe 3 (Just 7) `shouldBe` 7

    describe "fromList" $ do
        it "gets the Just first value from a list" $
            fromList [1..] `shouldBe` Just 1
        it "gets the Nothing from an empty list" $
            fromList ([] :: [Int]) `shouldBe` Nothing

    describe "isJust" $ do
        it "returns true if Just val" $
            isJust (Just 1) `shouldBe` True
        it "returns false if Nothing" $
            isJust Nothing `shouldBe` False

    describe "toMaybe" $ do
        it "returns Nothing if first argument is False" $
            toMaybe False 1 `shouldBe` Nothing
        it "returns Just val if first argument is True" $
            toMaybe True 1 `shouldBe` Just 1
