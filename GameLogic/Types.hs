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
           | DoorColor DistanceX
           | EmptyColor
           | PlayerColor
  deriving (Show, Eq)

data GridBead = Wall
              | Empty
              | Light { lightRadius :: Int
                      , lightColor :: (Int, Int, Int)
                      }
              | Door { doorMapName :: String
                     , doorId :: String
                     }
  deriving (Show, Eq)
