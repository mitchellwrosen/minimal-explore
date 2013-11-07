module GameLogic.GameMapSpec ( spec ) where

import Test.Hspec

import GameLogic.Types
    ( Door(..)
    , GridBead(..)
    , Light(..)
    , Facing(..)
    , makeColor
    )
import GameLogic.Move
    ( Move( MoveRight ) )
import GameLogic.GameMap
    ( makeGameMap
    , gameMapGrid
    , gameMapName
    , gameMapDoors
    , gameMapLights
    , gameMapAmbientLight
    , gameMapApplyMoveLight
    , getGameMapFromDoor
    , getMatchingDoorPosition
    )

import Control.Lens
    ( (^.)
    , (.~)
    )
import Control.Exception
    ( evaluate )

spec :: Spec
spec =
    describe "gameMap" $ do
        describe "light movement" $ do
            let name = "mapA"
                light = Light 255 $ makeColor (255, 255, 255)
                grid = [ [ [ LightBead light, Empty, LightBead light ] ] ]
                gameMap = makeGameMap grid name 255
                mapLight = head $ gameMap ^. gameMapLights

                grid' = [ [ [ Empty, LightBead light, LightBead light ] ] ]
                gameMap' = makeGameMap grid' name 255

            it "can move" $ do
                gameMapApplyMoveLight gameMap mapLight Positive MoveRight
                    `shouldBe` gameMap'

        describe "player steps on door" $ do
            let doorA = DoorBead $ Door "mapB" "unique" $ makeColor (255, 255, 255)
                gridA = [ [ [ doorA ] ] ]
                gameMapA = makeGameMap gridA "mapA" 255

                doorB = DoorBead $ Door "mapA" "unique" $ makeColor (255, 255, 255)
                gridB = [ [ [ doorB ] ] ]
                gameMapB = makeGameMap gridB "mapB" 255

                maps = [ (gameMapA^.gameMapName, gameMapA)
                       , (gameMapB^.gameMapName, gameMapB)
                       ]

                badDoor = DoorBead $ Door "mapC" "unique" $ makeColor (255, 255, 255)

            describe "finding a matching map" $ do
                it "finds the matching map" $
                    getGameMapFromDoor maps doorA `shouldBe` gameMapB

                it "throws an error if the map does not exist" $ do
                    evaluate (getGameMapFromDoor maps badDoor) `shouldThrow`
                        errorCall "Bad RoomName mapC"

            it "finds the matching door in a map" $ do
                getMatchingDoorPosition gameMapA gameMapB doorA `shouldBe` (0, 0, 0)

        let name = "mapA"
            door = Door "mapB" "unique" $ makeColor (255, 255, 255)
            grid = [ [ [ DoorBead door ] ] ]
            gameMap = makeGameMap grid name 255
        describe "lenses" $ do
            it "gameMapGrid" $ do
                let grid' = [[[]]]
                (gameMapGrid .~ grid' $ gameMap)^.gameMapGrid `shouldBe` grid'

            it "gameMapName" $ do
                let name' = "newName"
                (gameMapName .~ name' $ gameMap)^.gameMapName `shouldBe` name'

            it "gameMapDoors" $ do
                let doors = []
                (gameMapDoors .~ doors $ gameMap)^.gameMapDoors `shouldBe` doors

            it "gameMapLights" $ do
                let lights = []
                (gameMapLights .~ lights $ gameMap)^.gameMapLights `shouldBe` lights

            it "gameMapAmbeintLight" $ do
                let ambientLight = 3
                (gameMapAmbientLight .~ ambientLight $ gameMap)^.gameMapAmbientLight `shouldBe` ambientLight

        it "has a grid" $ do
            gameMap ^. gameMapGrid `shouldBe` grid

        it "has a name" $ do
            gameMap ^. gameMapName `shouldBe` name

        it "has a list of doors" $ do
            map fst (gameMap ^. gameMapDoors) `shouldBe` [ door ]
