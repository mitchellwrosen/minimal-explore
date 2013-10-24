module GameLogic.Color ( Color(..)
                       , BeadColor(..)
                       , toList
                       , fromList
                       ) where

import Prelude ( Int
               , Show
               , Eq
               )

type Color = (Int, Int, Int)

type DistanceX = Int
data BeadColor = WallColor DistanceX
               | DoorColor DistanceX
               | EmptyColor
               | PlayerColor
               | LightColor Color
  deriving (Show, Eq)

toList :: Color -> [Int]
toList (r, g, b) = [r, g, b]

fromList :: [Int] -> Color
fromList [r, g, b] = (r, g, b)
