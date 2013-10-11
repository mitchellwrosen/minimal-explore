module GameLogic.GameSpec where

import Test.Hspec
import Main.Types
import GameLogic.Grid ( replace
                      , gridGet
                      , gridSet
                      , Grid(..)
                      )

testMap :: Grid Int
testMap = [ [ [ 000, 001 ]
            , [ 010, 011 ]
            ]

          , [ [ 100, 101 ]
            , [ 110, 111 ]
            ]
          ]

spec :: Spec
spec = do
    describe "list replace" $ do
        it "replaces an element in a list" $ do
            replace ["a", "b", "c"] 1 "d" `shouldBe` ["a", "d", "c"]
            replace ["a", "b", "c"] 0 "d" `shouldBe` ["d", "b", "c"]
            replace ["a", "b", "c"] 2 "d" `shouldBe` ["a", "b", "d"]

    describe "the game logic" $ do
        describe "the grid" $ do
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
