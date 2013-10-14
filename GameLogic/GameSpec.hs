module GameLogic.GameSpec where

import Test.Hspec
import Main.Types
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , Grid(..)
                      )
import GameLogic.Player

data GridBead = Wall
              | Empty
              | PlayerBead Player
  deriving (Show, Eq)

getView :: Grid a -> Player -> [[a]]
getView grid player = grid !! playerX
  where
    (playerX, _, _) = playerGetPosition player

testPlayer :: Player
testPlayer = Player (1, 1, 1) Positive

spec :: Spec
spec = do
    describe "list replace" $ do
        it "replaces an element in a list" $ do
            replace ["a", "b", "c"] 1 "d" `shouldBe` ["a", "d", "c"]
            replace ["a", "b", "c"] 0 "d" `shouldBe` ["d", "b", "c"]
            replace ["a", "b", "c"] 2 "d" `shouldBe` ["a", "b", "d"]

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
            gridGet testGrid 0 1 1 `shouldBe` 011
            gridGet testGrid 1 1 0 `shouldBe` 110

        it "can set the value at a given (x,y,z)" $ do
            gridSet testGrid 1 1 0 42 `shouldBe`
                [ [ [ 000, 001 ]
                  , [ 010, 011 ]
                  ]

                , [ [ 100, 101 ]
                  , [ 42, 111 ]
                  ]
                ]

        it "can have players and walls and empties" $ do
            let grid :: Grid GridBead
                grid = [ [ [ PlayerBead testPlayer, Wall ]
                         , [ Wall,                  Wall ]
                         ]

                       , [ [ Empty, Wall ]
                         , [ Empty, Wall ]
                         ]
                       ]
            gridGet grid 0 0 0 `shouldBe` PlayerBead testPlayer
            gridGet grid 0 1 1 `shouldBe` Wall
            gridGet grid 1 0 0 `shouldBe` Empty

    describe "Game Logic" $ do
        describe "the player" $ do
            it "has an xyz position" $ do
                playerGetPosition testPlayer `shouldBe` (1, 1, 1)

            it "moves up and down in the y direction one unit" $ do
                playerGetPosition (playerMoveDown testPlayer) `shouldBe` (1, 0, 1)
                playerGetPosition (playerMoveUp testPlayer) `shouldBe` (1, 2, 1)

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
        let testGrid :: Grid Int
            testGrid = [ [ [ 000, 001, 002 ]
                         , [ 010, 011, 012 ]
                         , [ 020, 021, 022 ]
                         ]

                       , [ [ 100, 101, 102 ]
                         , [ 110, 111, 112 ]
                         , [ 120, 121, 122 ]
                         ]

                       , [ [ 100, 101, 102 ]
                         , [ 110, 111, 112 ]
                         , [ 120, 121, 122 ]
                         ]
                       ]

        it "returns the yz plane from the grid and the player" $ do
            getView testGrid testPlayer `shouldBe` testGrid !! 1
