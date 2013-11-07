module GameLogic.StateSpec ( spec ) where

import Test.Hspec

import GameLogic.Types
    ( GridBead(..)
    , Facing(..)
    , Door(..)
    , Gate(..)
    , Light(..)
    , Position
    , posZ
    , makeColor
    )
import GameLogic.Grid
    ( Grid )
import GameLogic.GameMap
    ( makeGameMap
    , gameMapLights
    )
import GameLogic.Types.Player
    ( makePlayer
    , playerPosition
    , playerFacing
    )
import GameLogic.Types.GameState
    ( GameState
    , makeGameState
    , gameStatePlayer
    , gameStateGameMaps
    , gameStateGameMap
    )
import GameLogic.State
    ( leftButtonPressed
    , rightButtonPressed
    , upButtonPressed
    , downButtonPressed
    , forwardButtonPressed
    , reverseButtonPressed
    )
import GameLogic.State.Internal
    ( loadNewRoom
    )

import Control.Lens
    ( (^.)
    , (.~)
    )

defLight :: Light
defLight = Light 0 $ makeColor (0, 0, 0)

spec :: Spec
spec = describe "grid state" $ do
          describe "gates" $ do
             let gridNoLightBead =  [ [ [ Empty, GateBead $ Gate $ makeColor (192, 192, 192) ] ] ]
                 gridLightBead =  [ [ [ Empty, LightBead $ Light 0 $ makeColor (255, 255, 255) , GateBead $ Gate $ makeColor (192, 192, 192) ] ] ]

                 gameState ambient playerPos grid = makeGameState (makePlayer playerPos Positive) gameMap gameMaps
                   where
                     gameMap = makeGameMap grid "map" ambient
                     gameMaps = [ ("map", gameMap) ]

             describe "gate closed" $ do
                 let ambient = 128
                     gameState' = gameState ambient (0, 0, 0)
                 it "player cannot walk through" $
                     rightButtonPressed False (gameState' gridNoLightBead)
                        `shouldBe` gameState' gridNoLightBead
                 it "player cannot push lightbead through" $
                     rightButtonPressed False (gameState' gridLightBead)
                        `shouldBe` gameState' gridLightBead

             describe "gate open" $ do
                 let ambient = 192
                     gameState' = gameState ambient
                 it "player can walk through" $
                     rightButtonPressed False (gameState' (0, 0, 0) gridNoLightBead)
                        `shouldBe` gameState ambient (0, 0, 1) gridNoLightBead
                 describe "player can push lightbead through" $ do
                     let gameState'' = rightButtonPressed False (gameState' (0, 0, 0) gridLightBead)
                     it "player moved" $ do
                         gameState''^.gameStatePlayer^.playerPosition^.posZ `shouldBe` 1
                     it "light moved" $ do
                         (snd . head $ gameState''^.gameStateGameMap^.gameMapLights)^.posZ `shouldBe` 2

          describe "load new room" $ do
             let gridA :: Grid GridBead
                 doorBeadA = DoorBead $ Door "mapB" "a" $ makeColor (0, 0, 0)
                 gridA =  [ [ [ doorBeadA ] ] ]
                 gameMapA = makeGameMap gridA "mapA" 255

                 gridB :: Grid GridBead
                 gridB =  [[[ Empty, DoorBead $ Door "mapA" "a" $ makeColor (0, 0, 0) ]]]
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

          let testGrid :: Grid GridBead
              testGrid = [ [ [ Wall,  Wall,  Wall ]
                           , [ Empty, Empty, Empty ]
                           , [ Empty, Empty, Empty ]
                           ]

                         , [ [ Wall,  Wall,  Wall ]
                           , [ Empty, Empty, Empty ]
                           , [ Empty, Empty, Empty ]
                           ]

                         , [ [ Empty,  Wall,  Wall ]
                           , [ LightBead defLight, LightBead defLight, Empty ]
                           , [ DoorBead $ Door "new map" "a" $ makeColor (255, 255, 255), Empty, Empty ]
                           ]
                         ]
              gridB = [ [ [ DoorBead $ Door "test" "a" $ makeColor (0, 0, 0) ] ] ]
              testMap = makeGameMap testGrid "test" 255
              mapB = makeGameMap gridB "new map" 255
              gameMaps = [ ("test", testMap)
                         , ("new map", mapB)
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

          describe "player movement" $ do
              it "allows player movement to a valid position" $ do
                  leftButtonPressed False (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 0)
              it "disallows player movement to an invalid position" $ do
                  leftButtonPressed False (gameState (1, 1, 0)) `shouldBe` gameState (1, 1, 0)

              describe "moves into light" $ do
                  describe "can't move" $ do
                      it "if light would be OoB" $
                          forwardButtonPressed False (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 1)
                      it "if light would run into another light" $
                          leftButtonPressed False (gameState (2, 1, 2)) `shouldBe` gameState (2, 1, 2)
                  describe "can move" $ do
                      it "moves the player" $
                          (upButtonPressed False (gameState (2, 2, 0)))^.gameStatePlayer^.playerPosition
                              `shouldBe` (2, 1, 0)
                      describe "moves the light" $ do
                          let gameState' = upButtonPressed False (gameState (2, 2, 0))
                              lights = (gameState')^.gameStateGameMap^.gameMapLights
                          it "has a new light position" $
                              (defLight, (2, 0, 0)) `elem` lights `shouldBe` True

              describe "moves into door" $ do
                  it "loads a new room" $
                      leftButtonPressed False (gameState (2, 2, 1))^.gameStateGameMap
                          `shouldBe` mapB

              describe "directions" $ do
                  it "left" $ do
                      leftButtonPressed False (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 0)
                  it "right" $ do
                      rightButtonPressed False (gameState (1, 1, 1)) `shouldBe` gameState (1, 1, 2)
                  it "up" $ do
                      upButtonPressed False (gameState (1, 2, 1)) `shouldBe` gameState (1, 1, 1)
                  it "down" $ do
                      downButtonPressed False (gameState (1, 1, 1)) `shouldBe` gameState (1, 2, 1)
                  it "forward" $ do
                      forwardButtonPressed False (gameState (0, 1, 1)) `shouldBe` gameState (1, 1, 1)
                  describe "reverse" $ do
                      let gameStateReverse :: Position -> GameState
                          gameStateReverse pos = makeGameState (makePlayer pos Negative) testMap gameMaps
                      it "change directions" $ do
                          reverseButtonPressed False (gameState (1, 1, 1)) `shouldBe`
                              gameStateReverse (1, 1, 1)
                      it "forward" $ do
                          forwardButtonPressed False (gameStateReverse (1, 1, 1)) `shouldBe`
                              gameStateReverse (0, 1, 1)
