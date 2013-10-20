module GameLogic.Types where

import Prelude ( Int
               , Show
               , Eq
               , String
               )

type GridX = Int
type GridY = Int
type GridZ = Int

type BeadData = Int
type MWheelDelta = Int
type KeyValue = Int

type BeadMap = [[[BeadData]]]

type DistanceX = Int
data Color = WallColor DistanceX
           | EmptyColor
           | PlayerColor
  deriving (Show, Eq)

data GridBead = Wall
              | Empty
              | Door { doorMapName :: String
                     , doorId :: String
                     }
  deriving (Show, Eq)
