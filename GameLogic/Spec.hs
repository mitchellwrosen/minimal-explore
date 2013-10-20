module GameLogic.Spec ( spec
                      ) where

import Test.Hspec

import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , GridBead(..)
                       , Color(..)
                       , Position(..)
                       )
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , Grid(..)
                      )
import GameLogic.Player ( Player(..)
                        , Facing(..)
                        , pFacing
                        , pPosition
                        , playerChangeDirection
                        , playerMoveUp
                        , playerMoveDown
                        , playerMoveLeft
                        , playerMoveRight
                        , playerMoveForward
                        )
import GameLogic.State ( GameState(..)
                       , isValidPlayerPosition
                       , leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       )
import GameLogic.View ( getView
                      )

import Control.Lens ( (^.) )

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
            gameState :: Position -> GameState
            gameState pos = GameState (Player pos Positive) testGrid

        describe "player positions" $ do
            describe "valid positions" $ do
                it "Empty slots" $ do
                    let gs = gameState (Position (1, 1, 1))
                    isValidPlayerPosition gs `shouldBe` True
            describe "invalid positions" $ do
                it "Wall slots" $ do
                    let gs = gameState (Position (1, 0, 1))
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB X" $ do
                    let gs = gameState (Position (3, 0, 1))
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB Y" $ do
                    let gs = gameState (Position (1, -1, 1))
                    isValidPlayerPosition gs `shouldBe` False
                it "OoB Z" $ do
                    let gs = gameState (Position (1, 1, 3))
                    isValidPlayerPosition gs `shouldBe` False

        describe "player movement" $ do
            it "allows player movement to a valid position" $ do
                leftButtonPressed (gameState (Position (1, 1, 1))) `shouldBe` gameState (Position (1, 1, 0))
            it "disallows player movement to an invalid position" $ do
                leftButtonPressed (gameState (Position (1, 1, 0))) `shouldBe` gameState (Position (1, 1, 0))

            describe "directions" $ do
                it "left" $ do
                    leftButtonPressed (gameState (Position (1, 1, 1))) `shouldBe` gameState (Position (1, 1, 0))
                it "right" $ do
                    rightButtonPressed (gameState (Position (1, 1, 1))) `shouldBe` gameState (Position (1, 1, 2))
                it "up" $ do
                    upButtonPressed (gameState (Position (1, 2, 1))) `shouldBe` gameState (Position (1, 1, 1))
                it "down" $ do
                    downButtonPressed (gameState (Position (1, 1, 1))) `shouldBe` gameState (Position (1, 2, 1))
                it "forward" $ do
                    forwardButtonPressed (gameState (Position (1, 1, 1))) `shouldBe` gameState (Position (2, 1, 1))
                describe "reverse" $ do
                    let gameStateReverse :: Position -> GameState
                        gameStateReverse pos = GameState (Player pos Negative) testGrid
                    it "change directions" $ do
                        reverseButtonPressed (gameState (Position (1, 1, 1))) `shouldBe`
                            gameStateReverse (Position (1, 1, 1))
                    it "forward" $ do
                        forwardButtonPressed (gameStateReverse (Position (1, 1, 1))) `shouldBe`
                            gameStateReverse (Position (0, 1, 1))

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
            gridGet testGrid (Position (0,1,1)) `shouldBe` Just 011
            gridGet testGrid (Position (1,1,0)) `shouldBe` Just 110

        it "can get Nothing out of bounds" $ do
            gridGet testGrid (Position (-1,1,1)) `shouldBe` Nothing
            gridGet testGrid (Position (0,1,2)) `shouldBe` Nothing

        it "can set the value at a given (x,y,z)" $ do
            gridSet testGrid (Position (1,1,0)) 42 `shouldBe`
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
            gridGet grid (Position (0,1,1)) `shouldBe` Just Wall
            gridGet grid (Position (1,0,0)) `shouldBe` Just Empty

    describe "the player" $ do
        let testPlayer :: Player
            testPlayer = Player (Position (1,1,1)) Positive
        it "has an xyz position" $ do
            testPlayer^.pPosition `shouldBe` Position (1,1,1)

        it "moves up and down in the y direction one unit" $ do
            (playerMoveDown testPlayer)^.pPosition `shouldBe` Position (1,2,1)
            (playerMoveUp testPlayer)^.pPosition `shouldBe` Position (1,0,1)

        it "moves left and right in the z direction one unit" $ do
            (playerMoveLeft testPlayer)^.pPosition `shouldBe` Position (1,1,0)
            (playerMoveRight testPlayer)^.pPosition `shouldBe` Position (1,1,2)

        it "can move forward in the x direction one unit" $ do
            (playerMoveForward testPlayer)^.pPosition `shouldBe` Position (2,1,1)

        it "faces +x direction" $ do
            testPlayer^.pFacing `shouldBe` Positive

        describe "changing direction" $ do
            let player' = playerChangeDirection testPlayer
            it "faces -x direction" $ do
                player'^.pFacing `shouldBe` Negative

            it "reverses left/right for the z direction" $ do
                (playerMoveLeft player')^.pPosition `shouldBe` Position (1,1,2)
                (playerMoveRight player')^.pPosition `shouldBe` Position (1,1,0)

            it "moves forward in the -x direction" $ do
                (playerMoveForward player')^.pPosition `shouldBe` Position (0,1,1)

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
            viewAt state x y = (getView state !! x) !! y

        {-
         -describe "the edge" $ do
         -    let gameState = GameState (Player (2, 0, 0) Positive) testGrid
         -    it "draws OoB with the wall color" $ do
         -        viewAt gameState 0 1 `shouldBe` WallColor 1
         -}

        describe "positive facing" $ do
            let gameState = GameState (Player (Position (1,0,2)) Positive) testGrid
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 1 1 `shouldBe` WallColor 0
            it "draws the player with player color with Positive facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
            it "draws empty walls with the faded WallColor" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 2

        describe "negative facing" $ do
            let gameState = GameState (Player (Position (2,0,0)) Negative) testGrid
            it "draws the player with player color with Negative facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws empties with the faded WallColor" $ do
                viewAt gameState 1 2 `shouldBe` WallColor 2
