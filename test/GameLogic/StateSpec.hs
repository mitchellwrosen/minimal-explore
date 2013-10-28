module GameLogic.StateSpec ( spec ) where

import Test.Hspec

import GameLogic.Types
import GameLogic.Grid
import GameLogic.GameMap
import GameLogic.State
import GameLogic.Player

spec :: Spec
spec =
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

            gameState :: Position -> GameState
            gameState pos = makeGameState (makePlayer pos Positive) testMap

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
                    let gameStateReverse :: Position -> GameState
                        gameStateReverse pos = makeGameState (makePlayer pos Negative) testMap
                    it "change directions" $ do
                        reverseButtonPressed (gameState (1, 1, 1)) `shouldBe`
                            gameStateReverse (1, 1, 1)
                    it "forward" $ do
                        forwardButtonPressed (gameStateReverse (1, 1, 1)) `shouldBe`
                            gameStateReverse (0, 1, 1)
