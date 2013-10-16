module GameLogic.Spec where

import Test.Hspec
import Main.Types
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , gridDimensions
                      , Grid(..)
                      )
import GameLogic.Player
import GameLogic.State
import GameLogic.View

spec :: Spec
spec = do
    describe "list replace" $ do
        it "replaces an element in a list" $ do
            replace ["a", "b", "c"] 1 "d" `shouldBe` ["a", "d", "c"]
            replace ["a", "b", "c"] 0 "d" `shouldBe` ["d", "b", "c"]
            replace ["a", "b", "c"] 2 "d" `shouldBe` ["a", "b", "d"]

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
            gameState :: (GridX, GridY, GridZ) -> GameState
            gameState pos = GameState (Player pos Positive) testGrid

        describe "player positions" $ do
            describe "valid positions" $ do
                it "Empty slots" $ do
                    let gs = gameState (1, 1, 1)
                    isValidPlayerPosition gs `shouldBe` True
            describe "invalid positions" $ do
                it "Wall slots" $ do
                    let gs = gameState (1, 0, 1)
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB X" $ do
                    let gs = gameState (3, 0, 1)
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB Y" $ do
                    let gs = gameState (1, (-1), 1)
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB Z" $ do
                    let gs = gameState (1, 1, 3)
                    isValidPlayerPosition gs `shouldBe` False

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

    describe "Game View" $ do
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
            viewAt :: GameState -> Int -> Int -> Color
            viewAt state x y = ((getView state) !! x) !! y

        {-
         -describe "the edge" $ do
         -    let gameState = GameState (Player (2, 0, 0) Positive) testGrid
         -    it "draws OoB with the wall color" $ do
         -        viewAt gameState 0 1 `shouldBe` WallColor 1
         -}

        describe "positive facing" $ do
            let gameState = GameState (Player (1, 0, 2) Positive) testGrid
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 1 1 `shouldBe` WallColor 0
            it "draws the player with player color with Positive facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
            it "draws empty walls with the faded WallColor" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 2

        describe "negative facing" $ do
            let gameState = GameState (Player (2, 0, 0) Negative) testGrid
            it "draws the player with player color with Negative facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws empties with the faded WallColor" $ do
                viewAt gameState 1 2 `shouldBe` WallColor 2
