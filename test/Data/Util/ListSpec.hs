module Data.Util.ListSpec ( spec ) where

import Test.Hspec
import Data.Util.List ( replace )

spec :: Spec
spec = do
    describe "list replace" $ do
        it "replaces an element in a list" $ do
            replace ["a", "b", "c"] 1 "d" `shouldBe` ["a", "d", "c"]
            replace ["a", "b", "c"] 0 "d" `shouldBe` ["d", "b", "c"]
            replace ["a", "b", "c"] 2 "d" `shouldBe` ["a", "b", "d"]
