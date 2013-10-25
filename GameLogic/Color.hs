module GameLogic.Color ( toList
                       , fromList
                       ) where

import Prelude ( Int
               , Show
               , Eq
               )

import GameLogic.Types ( Color
                       )


toList :: Color -> [Int]
toList (r, g, b) = [r, g, b]

fromList :: [Int] -> Color
fromList [r, g, b] = (r, g, b)
