module GameLogic.PlayerSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( Facing(..) )
import GameLogic.Move ( moveRight )

import GameLogic.Types.Player
    ( Player
    , makePlayer
    , playerPosition
    , playerFacing
    )
import GameLogic.Player ( playerApplyMove
                        , playerChangeDirection
                        )

import Control.Lens ( (^.) )

spec :: Spec
spec = 
    describe "the player" $ do
        let testPlayer :: Player
            testPlayer = makePlayer (1, 1, 1) Positive
        it "has an xyz position" $ do
            testPlayer ^. playerPosition `shouldBe` (1, 1, 1)

        it "moves right in the z direction one unit" $ do
            (playerApplyMove testPlayer moveRight) ^. playerPosition `shouldBe` (1, 1, 2)

        it "faces +x direction" $ do
            testPlayer ^. playerFacing `shouldBe` Positive

        describe "changing direction" $ do
            let player' = playerChangeDirection testPlayer
                player'' = playerChangeDirection player'
            it "faces -x direction" $ do
                player' ^. playerFacing `shouldBe` Negative

            it "faces +x direction" $ do
                player'' ^. playerFacing `shouldBe` Positive

            it "reverses left/right for the z direction" $ do
                (playerApplyMove player' moveRight) ^. playerPosition `shouldBe` (1, 1, 0)
