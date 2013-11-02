module Data.Util.MathSpec ( spec ) where

import Test.Hspec
import Data.Util.Math ( distance )

spec :: Spec
spec = describe "math" $ do
    describe "distance" $ do
        it "gets the Int distance between two points" $
            distance (0, 0, 0) (1, 0, 0) `shouldBe` 1
