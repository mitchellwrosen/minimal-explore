module GameLogic.TypesSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( Door(..)
                       , doorMapName
                       , doorId
                       , makeColor
                       , posX
                       , posY
                       , posZ
                       )

import Control.Lens ( (^.)
                    , (.~)
                    )

spec :: Spec
spec = do
    describe "door" $ do
        let door = Door "map" "id" $ makeColor (255, 255, 255)
        it "has a map name" $ do
            door^.doorMapName `shouldBe` "map"
        it "has a unqiue id" $ do
            door^.doorId `shouldBe` "id"

        describe "lenses" $ do
            it "doorMapName" $ do
                let name = "new name"
                (doorMapName.~name $ door)^.doorMapName `shouldBe` name
            it "doorId" $ do
                let ident = "new ident"
                (doorId.~ident $ door)^.doorId `shouldBe` ident
    describe "door" $ do
        describe "lenses" $ do
            let pos = (0, 0, 0)
            it "posX" $ do
                (posX.~1 $ pos) `shouldBe` (1, 0, 0)
                (posX.~1 $ pos)^.posX `shouldBe` 1
            it "posY" $ do
                (posY.~1 $ pos) `shouldBe` (0, 1, 0)
                (posY.~1 $ pos)^.posY `shouldBe` 1
            it "posZ" $ do
                (posZ.~1 $ pos) `shouldBe` (0, 0, 1)
                (posZ.~1 $ pos)^.posZ `shouldBe` 1
