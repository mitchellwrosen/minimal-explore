module GameLogic.MoveSpec ( spec ) where

import Test.Hspec

import GameLogic.Types ( Facing(..)
                       )
import GameLogic.Move ( moveForward
                      , moveUp
                      , moveDown
                      , moveLeft
                      , moveRight
                      )

spec :: Spec
spec = describe "moves" $ do
    describe "positive" $ do
        let doMove move = move Positive (0, 0, 0)
        it "moveUp" $
            (doMove moveUp) `shouldBe` (0, -1, 0)
        it "moveDown" $
            (doMove moveDown) `shouldBe` (0, 1, 0)
        it "moveRight" $
            (doMove moveRight) `shouldBe` (0, 0, 1)
        it "moveLeft" $
            (doMove moveLeft) `shouldBe` (0, 0, -1)
        it "moveForward" $
            (doMove moveForward) `shouldBe` (1, 0, 0)

    describe "negative" $ do
        let doMove move = move Negative (0, 0, 0)
        it "moveUp" $
            (doMove moveUp) `shouldBe` (0, -1, 0)
        it "moveDown" $
            (doMove moveDown) `shouldBe` (0, 1, 0)
        it "moveRight" $
            (doMove moveRight) `shouldBe` (0, 0, -1)
        it "moveLeft" $
            (doMove moveLeft) `shouldBe` (0, 0, 1)
        it "moveForward" $
            (doMove moveForward) `shouldBe` (-1, 0, 0)
