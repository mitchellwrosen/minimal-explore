module GameLogic.GridSpec ( spec ) where

import Test.Hspec

import GameLogic.Types
import GameLogic.Grid

spec :: Spec
spec =
    describe "grid" $ do
        let testGrid :: Grid Int
            testGrid = [ [ [ 000, 001 ]
                         , [ 010, 011 ]
                         ]

                       , [ [ 100, 101 ]
                         , [ 110, 111 ]
                         ]
                       ]
        it "can get the value at a given (x,y,z)" $ do
            gridGet testGrid (0, 1, 1) `shouldBe` Just 011
            gridGet testGrid (1, 1, 0) `shouldBe` Just 110

        it "can get Nothing out of bounds" $ do
            gridGet testGrid ((-1), 1, 1) `shouldBe` Nothing
            gridGet testGrid (0, 1, 2) `shouldBe` Nothing

        it "can set the value at a given (x,y,z)" $ do
            gridSet testGrid (1, 1, 0) 42 `shouldBe`
                [ [ [ 000, 001 ]
                  , [ 010, 011 ]
                  ]

                , [ [ 100, 101 ]
                  , [ 42, 111 ]
                  ]
                ]

        it "can have walls and empties" $ do
            let grid :: Grid GridBead
                grid = [ [ [ Wall, Wall ]
                         , [ Wall, Wall ]
                         ]

                       , [ [ Empty, Wall ]
                         , [ Empty, Wall ]
                         ]
                       ]
            gridGet grid (0, 1, 1) `shouldBe` Just Wall
            gridGet grid (1, 0, 0) `shouldBe` Just Empty
