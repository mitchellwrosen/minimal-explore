module GameLogic.Types where

import Prelude ( Int
               , Show
               , Eq
               )

import Control.Lens (Lens(..))

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
  deriving (Show, Eq)

newtype Position = Position (GridX, GridY, GridZ) 
                 deriving (Show, Eq)

posX :: Lens Position GridX
posX = Lens (\(Position (x,_,_)) -> x) (\x (Position (_,y,z)) -> Position (x,y,z))

posY :: Lens Position GridY
posY = Lens (\(Position (_,y,_)) -> y) (\y (Position (x,_,z)) -> Position (x,y,z))

posZ :: Lens Position GridZ
posZ = Lens (\(Position (_,_,z)) -> z) (\z (Position (x,y,_)) -> Position (x,y,z))
