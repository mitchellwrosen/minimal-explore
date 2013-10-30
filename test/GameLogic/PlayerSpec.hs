module GameLogic.PlayerSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( Facing(..) )
import GameLogic.Move ( moveForward
                      , moveUp
                      , moveDown
                      , moveLeft
                      , moveRight
                      )

import GameLogic.Player ( Player
                        , makePlayer
                        , playerApplyMove
                        , playerPosition
                        , playerFacing
                        , playerChangeDirection
                        )

import Control.Lens ( (^.) )

-- TODO(R): split out into a MoveSpec.hs
spec :: Spec
spec = 
    describe "the player" $ do
        let testPlayer :: Player
            testPlayer = makePlayer (1, 1, 1) Positive
        it "has an xyz position" $ do
            testPlayer ^. playerPosition `shouldBe` (1, 1, 1)

        it "moves up and down in the y direction one unit" $ do
            (playerApplyMove testPlayer moveDown) ^. playerPosition `shouldBe` (1, 2, 1)
            (playerApplyMove testPlayer moveUp) ^. playerPosition `shouldBe` (1, 0, 1)

        it "moves left and right in the z direction one unit" $ do
            (playerApplyMove testPlayer moveLeft) ^. playerPosition `shouldBe` (1, 1, 0)
            (playerApplyMove testPlayer moveRight) ^. playerPosition `shouldBe` (1, 1, 2)

        it "can move forward in the x direction one unit" $ do
            (playerApplyMove testPlayer moveForward) ^. playerPosition `shouldBe` (2, 1, 1)

        it "faces +x direction" $ do
            testPlayer ^. playerFacing `shouldBe` Positive

        describe "changing direction" $ do
            let player' = playerChangeDirection testPlayer
            it "faces -x direction" $ do
                player' ^. playerFacing `shouldBe` Negative

            it "reverses left/right for the z direction" $ do
                (playerApplyMove player' moveLeft) ^. playerPosition `shouldBe` (1, 1, 2)
                (playerApplyMove player' moveRight) ^. playerPosition `shouldBe` (1, 1, 0)

            it "moves forward in the -x direction" $ do
                (playerApplyMove player' moveForward) ^. playerPosition `shouldBe` (0, 1, 1)
