module GameLogic.Spec ( spec
                      ) where

import Test.Hspec

import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , GridBead(..)
                       , Color(..)
                       )
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , gridElems
                      , Grid(..)
                      )
import GameLogic.Player ( Player(..)
                        , Facing(..)
                        , playerGetPosition
                        , playerGetFacing
                        , playerChangeDirection
                        , playerMoveUp
                        , playerMoveDown
                        , playerMoveLeft
                        , playerMoveRight
                        , playerMoveForward
                        )
import GameLogic.State ( GameState(..)
                       , leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       )
import GameLogic.View.Internal ( getView
                               , multLights
                               , lightIntensity
                               , getR
                               , getG
                               , getB
                               )
import GameLogic.GameMap ( GameMap(..)
                         , getGameMapFromDoor
                         , makeGameMap
                         , getMatchingDoorPosition
                         )

spec :: Spec
spec = do
    describe "list replace" $ do
        it "replaces an element in a list" $ do
            replace ["a", "b", "c"] 1 "d" `shouldBe` ["a", "d", "c"]
            replace ["a", "b", "c"] 0 "d" `shouldBe` ["d", "b", "c"]
            replace ["a", "b", "c"] 2 "d" `shouldBe` ["a", "b", "d"]

    describe "door" $ do
        let door = Door "map" "id"
        it "has a map name" $ do
            doorMapName door `shouldBe` "map"

        it "has a unqiue id" $ do
            doorId door `shouldBe` "id"

        describe "player steps on door" $ do
            let doorA = Door "mapB" "unique"
                gridA = [ [ [ doorA ] ] ]
                gameMapA = makeGameMap gridA "mapA" 255

                doorB = Door "mapA" "unique"
                gridB = [ [ [ doorB ] ] ]
                gameMapB = makeGameMap gridB "mapB" 255

                maps = [ (gameMapName gameMapA, gameMapA)
                       , (gameMapName gameMapB, gameMapB)
                       ]

            it "finds the matching map" $ do
                getGameMapFromDoor maps doorA `shouldBe` gameMapB

            it "finds the matching door in a map" $ do
                getMatchingDoorPosition gameMapA gameMapB doorA `shouldBe` (0, 0, 0)

    describe "gameMap" $ do
        let name = "mapA"
            door = Door "mapB" "unique"
            grid = [ [ [ door ] ] ]
            gameMap = makeGameMap grid name 255

        it "has a grid" $ do
            gameMapGrid gameMap `shouldBe` grid

        it "has a name" $ do
            gameMapName gameMap `shouldBe` name

        it "has a list of doors" $ do
            map fst (gameMapDoors gameMap) `shouldBe` [ door ]

    describe "grid state" $ do
        let testGrid :: Grid GridBead
            testGrid = [ [ [ Wall,  Wall,  Wall ]
                         , [ Empty, Empty, Empty ]
                         , [ Empty, Empty, Empty ]
                         ]

                       , [ [ Wall,  Wall,  Wall ]
                         , [ Empty, Empty, Empty ]
                         , [ Empty, Empty, Empty ]
                         ]

                       , [ [ Wall,  Wall,  Wall ]
                         , [ Empty, Empty, Empty ]
                         , [ Empty, Empty, Empty ]
                         ]
                       ]
            testMap = makeGameMap testGrid "test" 255

            gameState :: (GridX, GridY, GridZ) -> GameState
            gameState pos = GameState (Player pos Positive) testMap

        describe "player movement" $ do
            it "allows player movement to a valid position" $ do
                leftButtonPressed (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 0)
            it "disallows player movement to an invalid position" $ do
                leftButtonPressed (gameState (1, 1, 0)) `shouldBe` gameState (1, 1, 0)

            describe "directions" $ do
                it "left" $ do
                    leftButtonPressed (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 0)
                it "right" $ do
                    rightButtonPressed (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 2)
                it "up" $ do
                    upButtonPressed (gameState (1, 2, 1)) `shouldBe` gameState (1, 1, 1)
                it "down" $ do
                    downButtonPressed (gameState (1, 1, 1)) `shouldBe` gameState (1, 2, 1)
                it "forward" $ do
                    forwardButtonPressed (gameState (1, 1, 1)) `shouldBe` gameState (2, 1, 1)
                describe "reverse" $ do
                    let gameStateReverse :: (GridX, GridY, GridZ) -> GameState
                        gameStateReverse pos = GameState (Player pos Negative) testMap
                    it "change directions" $ do
                        reverseButtonPressed (gameState (1, 1, 1)) `shouldBe`
                            gameStateReverse (1, 1, 1)
                    it "forward" $ do
                        forwardButtonPressed (gameStateReverse (1, 1, 1)) `shouldBe`
                            gameStateReverse (0, 1, 1)

    describe "grid" $ do
        let testGrid :: Grid Int
            testGrid = [ [ [ 000, 001 ]
                         , [ 010, 011 ]
                         ]

                       , [ [ 100, 101 ]
                         , [ 110, 111 ]
                         ]
                       ]
        it "can get the value at a given (x,y,z)" $ do
            gridGet testGrid 0 1 1 `shouldBe` Just 011
            gridGet testGrid 1 1 0 `shouldBe` Just 110

        it "can get Nothing out of bounds" $ do
            gridGet testGrid (-1) 1 1 `shouldBe` Nothing
            gridGet testGrid   0  1 2 `shouldBe` Nothing

        it "can set the value at a given (x,y,z)" $ do
            gridSet testGrid 1 1 0 42 `shouldBe`
                [ [ [ 000, 001 ]
                  , [ 010, 011 ]
                  ]

                , [ [ 100, 101 ]
                  , [ 42, 111 ]
                  ]
                ]

        it "can have walls and empties" $ do
            let grid :: Grid GridBead
                grid = [ [ [ Wall, Wall ]
                         , [ Wall, Wall ]
                         ]

                       , [ [ Empty, Wall ]
                         , [ Empty, Wall ]
                         ]
                       ]
            gridGet grid 0 1 1 `shouldBe` Just Wall
            gridGet grid 1 0 0 `shouldBe` Just Empty

    describe "the player" $ do
        let testPlayer :: Player
            testPlayer = Player (1, 1, 1) Positive
        it "has an xyz position" $ do
            playerGetPosition testPlayer `shouldBe` (1, 1, 1)

        it "moves up and down in the y direction one unit" $ do
            playerGetPosition (playerMoveDown testPlayer) `shouldBe` (1, 2, 1)
            playerGetPosition (playerMoveUp testPlayer) `shouldBe` (1, 0, 1)

        it "moves left and right in the z direction one unit" $ do
            playerGetPosition (playerMoveLeft testPlayer) `shouldBe` (1, 1, 0)
            playerGetPosition (playerMoveRight testPlayer) `shouldBe` (1, 1, 2)

        it "can move forward in the x direction one unit" $ do
            playerGetPosition (playerMoveForward testPlayer) `shouldBe` (2, 1, 1)

        it "faces +x direction" $ do
            playerGetFacing testPlayer `shouldBe` Positive

        describe "changing direction" $ do
            let player' = playerChangeDirection testPlayer
            it "faces -x direction" $ do
                playerGetFacing player' `shouldBe` Negative

            it "reverses left/right for the z direction" $ do
                playerGetPosition (playerMoveLeft player') `shouldBe` (1, 1, 2)
                playerGetPosition (playerMoveRight player') `shouldBe` (1, 1, 0)

            it "moves forward in the -x direction" $ do
                playerGetPosition (playerMoveForward player') `shouldBe` (0, 1, 1)


    describe "GameView" $ do
        let testGrid :: Grid GridBead
            testGrid = [ [ [ Wall, Wall, Wall ]
                         , [ Wall, Empty, Wall ]
                         , [ Wall, Wall, Wall ]
                         ]

                       , [ [ Empty, Empty, Empty ]
                         , [ Empty, Wall, Empty ]
                         , [ Empty, Empty, Empty ]
                         ]

                       , [ [ Empty, Empty, Wall ]
                         , [ Empty, Empty, Wall ]
                         , [ Empty, Empty, Wall ]
                         ]
                       ]

            testMap = makeGameMap testGrid "test" 255
            viewAt :: GameState -> Int -> Int -> Color
            viewAt state x y = map fst (getView state !! x) !! y

        describe "lightIntensity" $ do
            let light dist = (Light 3 (255, 0, 0), dist)
            it "is at 100% when distance is 0" $ do
                --lightIntensity :: ((GridBead, Int) -> Int) -> (GridBead, Int) -> Double
                lightIntensity getR (light 0) `shouldBe` 255.0

            it "is at 0% when distance is greater than 3" $ do
                --lightIntensity :: ((GridBead, Int) -> Int) -> (GridBead, Int) -> Double
                lightIntensity getR (light 4) `shouldBe` 0.0

        describe "multLights" $ do
            let ambient@[ar, ag, ab] = [8, 9, 10]
                lights = [ (Light 3 (255, 255, 255), 2) ]
                [(ir, ig, ib)] =
                    map (\light -> (lightIntensity getR light,
                                    lightIntensity getG light,
                                    lightIntensity getB light)) lights
            it "0 diffuse means no extra light" $ do
                multLights (0.0, 0.0, 0.0) ambient lights `shouldBe` ambient

            it "1.0 diffuse means 100% light" $ do
                multLights (1.0, 0.0, 1.0) ambient lights `shouldBe`
                    map round [fromIntegral ar + ir, fromIntegral ag, fromIntegral ab + ib]

        describe "positive facing" $ do
            let gameState = GameState (Player (1, 0, 2) Positive) testMap
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 1 1 `shouldBe` WallColor 0
            it "draws the player with player color with Positive facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
            it "draws empty walls with the faded WallColor" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 2

        describe "negative facing" $ do
            let gameState = GameState (Player (2, 0, 0) Negative) testMap
            it "draws the player with player color with Negative facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws empties with the faded WallColor" $ do
                viewAt gameState 1 2 `shouldBe` WallColor 2
