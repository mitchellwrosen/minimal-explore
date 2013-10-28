module GameLogic.GameMapSpec ( spec ) where

import Test.Hspec

import GameLogic.Types
import GameLogic.Move
import GameLogic.GameMap
import Control.Lens

spec :: Spec
spec =
    describe "gameMap" $ do
        let name = "mapA"
            door = Door "mapB" "unique" (255, 255, 255)
            grid = [ [ [ DoorBead door ] ] ]
            gameMap = makeGameMap grid name 255

        it "has a grid" $ do
            gameMap ^. gameMapGrid `shouldBe` grid

        it "has a name" $ do
            gameMap ^. gameMapName `shouldBe` name

        it "has a list of doors" $ do
            map fst (gameMap ^. gameMapDoors) `shouldBe` [ door ]

        describe "light movement" $ do
            let name = "mapA"
                light = Light 255 (255, 255, 255)
                grid = [ [ [ LightBead light, Empty ] ] ]
                gameMap = makeGameMap grid name 255
                [mapLight] = gameMap ^. gameMapLights

                grid' = [ [ [ Empty, LightBead light ] ] ]
                gameMap' = makeGameMap grid' name 255

            it "can move" $ do
                gameMapApplyMoveLight gameMap mapLight Positive moveRight
                    `shouldBe` gameMap'

        describe "player steps on door" $ do
            let doorA = DoorBead $ Door "mapB" "unique" (255, 255, 255)
                gridA = [ [ [ doorA ] ] ]
                gameMapA = makeGameMap gridA "mapA" 255

                doorB = DoorBead $ Door "mapA" "unique" (255, 255, 255)
                gridB = [ [ [ doorB ] ] ]
                gameMapB = makeGameMap gridB "mapB" 255

                maps = [ (gameMapA^.gameMapName, gameMapA)
                       , (gameMapB^.gameMapName, gameMapB)
                       ]

            it "finds the matching map" $ do
                getGameMapFromDoor maps doorA `shouldBe` gameMapB

            it "finds the matching door in a map" $ do
                getMatchingDoorPosition gameMapA gameMapB doorA `shouldBe` (0, 0, 0)
