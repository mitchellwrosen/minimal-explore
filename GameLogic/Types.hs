module GameLogic.Types where

import Prelude ( Int
               , Show
               , Eq
               , String
               )

import GameLogic.Color ( Color(..)
                       )

type GridX = Int
type GridY = Int
type GridZ = Int

type BeadData = Int
type MWheelDelta = Int
type KeyValue = Int

type BeadMap = [[[BeadData]]]

data Light = Light { lightRadius :: Int
                   , lightColor :: Color
                   }
  deriving (Show, Eq)

data Door = Door { doorMapName :: String
                 , doorId :: String
                 }
  deriving (Show, Eq)

data GridBead = Wall
              | Empty
              | LightBead Light
              | DoorBead Door
  deriving (Show, Eq)
