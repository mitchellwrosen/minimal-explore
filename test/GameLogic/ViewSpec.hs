module GameLogic.ViewSpec ( spec ) where

import Test.Hspec

import GameLogic.Types.Player
    ( makePlayer )
import GameLogic.Types ( GridBead(..)
                       , BeadColor(..)
                       , Light(..)
                       , Facing(..)
                       )
import qualified GameLogic.Color as Color
import GameLogic.Grid ( Grid )
import GameLogic.GameMap ( makeGameMap )
import GameLogic.Types.GameState
    ( GameState
    , makeGameState
    )
import GameLogic.View.Internal ( getView
                               , lightIntensity
                               , phongLighting
                               )
import GameLogic.View.Light ( beadDiffuse )

import qualified Levels.GameMaps

spec :: Spec
spec =
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
            viewAt :: GameState -> Int -> Int -> BeadColor
            viewAt state x y = map fst (getView state !! x) !! y

        describe "lightIntensity" $ do
            let light dist = (Light 3 (255, 0, 0), dist)
                ir (r, _, _) = r
            it "is at 100% when distance is 0" $ do
                ir (lightIntensity $ light 0) `shouldBe` 255.0

            it "is at 0% when distance is greater than 3" $ do
                ir (lightIntensity $ light 4) `shouldBe` 0.0

        describe "beadDiffuse" $ do
            it "PlayerColor" $
                beadDiffuse PlayerColor `shouldBe` (1.0, 0.1, 0.1)
            it "DoorColor" $
                beadDiffuse (DoorColor 0) `shouldBe` (1.0, 1.0, 1.0)
            it "WallColor" $
                beadDiffuse (WallColor 0) `shouldBe` (0.1, 0.1, 0.1)

        describe "phongLighting" $ do
            let ambient@(ar, ag, ab) = (8, 9, 10)
                lights = [ (Light 3 (255, 255, 255), 2) ]
                [(ir, _, ib)] = map lightIntensity lights
            it "0 diffuse means no extra light" $ do
                phongLighting (0.0, 0.0, 0.0) ambient lights `shouldBe` ambient

            it "1.0 diffuse means 100% light" $ do
                phongLighting (1.0, 0.0, 1.0) ambient lights `shouldBe`
                    Color.fromList (map round [fromIntegral ar + ir, fromIntegral ag, fromIntegral ab + ib])

        describe "positive facing" $ do
            let gameState = makeGameState (makePlayer (1, 0, 2) Positive) testMap Levels.GameMaps.gameMaps
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 1 1 `shouldBe` WallColor 0
            it "draws the player with player color with Positive facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
            it "draws empty walls with the faded WallColor" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 2

        describe "negative facing" $ do
            let gameState = makeGameState (makePlayer (2, 0, 0) Negative) testMap Levels.GameMaps.gameMaps
            it "draws the player with player color with Negative facing" $ do
                viewAt gameState 0 2 `shouldBe` PlayerColor
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws walls with the wall foreground color" $ do
                viewAt gameState 0 0 `shouldBe` WallColor 0
            it "draws empties with the faded WallColor" $ do
                viewAt gameState 1 2 `shouldBe` WallColor 2
