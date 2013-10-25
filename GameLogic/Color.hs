module GameLogic.Color ( toList
                       , fromList
                       ) where

import Prelude ( Int
               , Show
               , Eq
               )

import GameLogic.Types ( Color
                       , Byte
                       )

toList :: Color -> [Byte]
toList (r, g, b) = [r, g, b]

fromList :: [Byte] -> Color
fromList [r, g, b] = (r, g, b)
