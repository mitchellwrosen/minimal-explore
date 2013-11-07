module GameLogic.ColorSpec ( spec ) where

import Test.Hspec
import GameLogic.Types ( BeadColor(..)
                       , makeColor
                       )
import GameLogic.Color

spec :: Spec
spec = describe "Color" $ do
    it "toList converts a color to a list" $
        toList (makeColor (255, 0, 128)) `shouldBe` [255, 0, 128]
    it "fromList converts a list to a color" $
        fromList [255, 0, 128] `shouldBe` makeColor (255, 0, 128)

    describe "ambientColor" $ do
        let maxLight = 128
        it "EmptyColor" $
            ambientColor maxLight EmptyColor `shouldBe` makeColor (maxLight, maxLight, maxLight)
        it "DoorColor" $
            ambientColor maxLight (DoorColor 3) `shouldBe` makeColor (maxLight, maxLight, maxLight)
        it "PlayerColor" $
            ambientColor maxLight PlayerColor `shouldBe` makeColor (maxLight, 0, 0)
        describe "WallColor" $ do
            it "it is black at distance 0" $
                ambientColor maxLight (WallColor 0) `shouldBe` makeColor (0, 0, 0)
            it "gets brighter as distance increases" $
                ambientColor maxLight (WallColor 3) `shouldBe` makeColor (96, 96, 96)
