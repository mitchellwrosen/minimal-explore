module GameLogic.TypesSpec ( spec ) where

import Test.Hspec
import GameLogic.Types
import Control.Lens

spec :: Spec
spec = do
    describe "door" $ do
        let door = Door "map" "id" (255, 255, 255)
        it "has a map name" $ do
            door^.doorMapName `shouldBe` "map"

        it "has a unqiue id" $ do
            door^.doorId `shouldBe` "id"
