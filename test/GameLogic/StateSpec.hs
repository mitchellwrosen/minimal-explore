module GameLogic.StateSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( GridBead(..)
                       , Facing(..)
                       , Door(..)
                       , Position
                       , posZ
                       )
import GameLogic.Grid ( Grid(..) )
import GameLogic.GameMap ( makeGameMap )
import GameLogic.Player ( makePlayer
                        , playerPosition
                        , playerFacing
                        )
import GameLogic.State ( GameState
                       , makeGameState
                       , gameStatePlayer
                       , gameStateGameMaps
                       , gameStateGameMap
                       , leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       )
import GameLogic.State.Internal ( loadNewRoom
                                , processLightMove
                                )

import Control.Lens ( (^.)
                    , (.~)
                    )

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
            gameMaps = [ ("test", testMap)
                       ]

            gameState :: Position -> GameState
            gameState pos = makeGameState (makePlayer pos Positive) testMap gameMaps

        describe "lenses" $ do
            it "gameStatePlayer" $ do
                let player = makePlayer (0, 0, 0) Negative
                (gameStatePlayer .~ player $ gameState (1, 1, 1))^.gameStatePlayer `shouldBe` player
            it "gameStateGameMap" $ do
                let gameMap = makeGameMap [[[]]] "game" 128
                (gameStateGameMap .~ gameMap $ gameState (1, 1, 1))^.gameStateGameMap `shouldBe` gameMap
            it "gameStateGameMaps" $ do
                let gameMaps' = []
                (gameStateGameMaps .~ gameMaps' $ gameState (1, 1, 1))^.gameStateGameMaps `shouldBe` gameMaps'

        describe "light movement" $ do
            it "this is a test" $
                False `shouldBe` True

        describe "load new room" $ do
            let gridA :: Grid GridBead
                doorBeadA = DoorBead $ Door "mapB" "a" (0, 0, 0)
                gridA =  [ [ [ doorBeadA ] ] ]
                gameMapA = makeGameMap gridA "mapA" 255

                gridB :: Grid GridBead
                gridB =  [[[ Empty, DoorBead $ Door "mapA" "a" (0, 0, 0) ]]]
                gameMapB = makeGameMap gridB "mapB" 255

                gameMaps = [ ("mapA", gameMapA)
                           , ("mapB", gameMapB)
                           ]

                gameState = makeGameState (makePlayer (0, 0, 0) Positive) gameMapA gameMaps

            it "should be gameMapA" $ do
                gameState^.gameStateGameMap `shouldBe` gameMapA

            describe "returns a new game state given the current game state and a doorbead" $ do
                let gameState' = loadNewRoom gameState doorBeadA
                it "should retain the old gameMaps" $
                    gameState'^.gameStateGameMaps `shouldBe` gameMaps
                it "should be the new map" $
                    gameState'^.gameStateGameMap `shouldBe` gameMapB
                it "should keep the player's Facing" $
                    gameState'^.gameStatePlayer^.playerFacing `shouldBe` Positive
                it "should set the player at the new door position" $
                    gameState'^.gameStatePlayer^.playerPosition^.posZ `shouldBe` 1

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
                        gameStateReverse pos = makeGameState (makePlayer pos Negative) testMap gameMaps
                    it "change directions" $ do
                        reverseButtonPressed (gameState (1, 1, 1)) `shouldBe`
                            gameStateReverse (1, 1, 1)
                    it "forward" $ do
                        forwardButtonPressed (gameStateReverse (1, 1, 1)) `shouldBe`
                            gameStateReverse (0, 1, 1)
