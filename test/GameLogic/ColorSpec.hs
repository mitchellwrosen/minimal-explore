module GameLogic.ColorSpec ( spec ) where

import Test.Hspec
import GameLogic.Types ( BeadColor(..)
                       )
import GameLogic.Color

spec :: Spec
spec = describe "Color" $ do
    it "toList converts a color to a list" $
        toList (255, 0, 128) `shouldBe` [255, 0, 128]
    it "fromList converts a list to a color" $
        fromList [255, 0, 128] `shouldBe` (255, 0, 128)

    describe "ambientColor" $ do
        let maxLight = 128
        it "EmptyColor" $
            ambientColor maxLight EmptyColor `shouldBe` (maxLight, maxLight, maxLight)
        it "DoorColor" $
            ambientColor maxLight (DoorColor 3) `shouldBe` (maxLight, maxLight, maxLight)
        it "PlayerColor" $
            ambientColor maxLight PlayerColor `shouldBe` (maxLight, 0, 0)
        describe "WallColor" $ do
            it "it is black at distance 0" $
                ambientColor maxLight (WallColor 0) `shouldBe` (0, 0, 0)
            it "gets brighter as distance increases" $
                ambientColor maxLight (WallColor 3) `shouldBe` (96, 96, 96)
