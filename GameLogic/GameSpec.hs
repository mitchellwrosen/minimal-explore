module GameLogic.GameSpec where

import Test.Hspec
import Main.Types
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , Grid(..)
                      )
import GameLogic.Player



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
        let testMap :: Grid Int
            testMap = [ [ [ 000, 001 ]
                        , [ 010, 011 ]
                        ]

                      , [ [ 100, 101 ]
                        , [ 110, 111 ]
                        ]
                      ]
        it "can get the value at a given (x,y,z)" $ do
            gridGet testMap 0 1 1 `shouldBe` 011
            gridGet testMap 1 1 0 `shouldBe` 110

        it "can set the value at a given (x,y,z)" $ do
            gridSet testMap 1 1 0 42 `shouldBe`
                [ [ [ 000, 001 ]
                  , [ 010, 011 ]
                  ]

                , [ [ 100, 101 ]
                  , [ 42, 111 ]
                  ]
                ]

    describe "GameLogic" $ do
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
