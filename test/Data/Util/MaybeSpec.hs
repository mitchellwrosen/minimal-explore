module Data.Util.MaybeSpec ( spec ) where

import Test.Hspec
import Data.Util.Maybe

spec :: Spec
spec = describe "Maybe" $ do
    describe "fromMaybe" $ do
        it "uses the default value if Nothing" $
            fromMaybe "a" Nothing `shouldBe` "a"
        it "uses value if Just value" $
            fromMaybe "a" (Just "b") `shouldBe` "b"

    describe "fromList" $ do
        it "gets the Just first value from a list" $
            fromList "hello" `shouldBe` Just 'h'
        it "gets the Nothing from an empty list" $
            fromList ([] :: [Int]) `shouldBe` Nothing

    describe "isJust" $ do
        it "returns true if Just val" $
            isJust (Just 'a') `shouldBe` True
        it "returns false if Nothing" $
            isJust Nothing `shouldBe` False

    describe "toMaybe" $ do
        it "returns Nothing if first argument is False" $
            toMaybe False 'a' `shouldBe` Nothing
        it "returns Just val if first argument is True" $
            toMaybe True 'a' `shouldBe` Just 'a'
