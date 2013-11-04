module GameLogic.MoveSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( Facing(..)
                       )
import GameLogic.Move ( Move(..)
                      , applyMove
                      )

spec :: Spec
spec = describe "moves" $ do
    describe "positive" $ do
        let doMove move = applyMove move Positive (0, 0, 0)
        it "moveUp" $
            (doMove MoveUp) `shouldBe` (0, -1, 0)
        it "MoveDown" $
            (doMove MoveDown) `shouldBe` (0, 1, 0)
        it "MoveRight" $
            (doMove MoveRight) `shouldBe` (0, 0, 1)
        it "MoveLeft" $
            (doMove MoveLeft) `shouldBe` (0, 0, -1)
        it "MoveForward" $
            (doMove MoveForward) `shouldBe` (1, 0, 0)

    describe "negative" $ do
        let doMove move = applyMove move Negative (0, 0, 0)
        it "MoveUp" $
            (doMove MoveUp) `shouldBe` (0, -1, 0)
        it "MoveDown" $
            (doMove MoveDown) `shouldBe` (0, 1, 0)
        it "MoveRight" $
            (doMove MoveRight) `shouldBe` (0, 0, -1)
        it "MoveLeft" $
            (doMove MoveLeft) `shouldBe` (0, 0, 1)
        it "MoveForward" $
            (doMove MoveForward) `shouldBe` (-1, 0, 0)
