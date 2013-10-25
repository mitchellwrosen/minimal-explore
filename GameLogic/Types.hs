module GameLogic.Types where

import Prelude ( Int
               , Show
               , Eq
               , String
               )
import Control.Lens ( Lens(..)
                    )

type GridX = Int
type GridY = Int
type GridZ = Int

type BeadData = Int
type MWheelDelta = Int
type KeyValue = Int

type BeadMap = [[[BeadData]]]

type Byte = Int

data Light = Light { lightRadius :: Int
                   , lightColor :: Color
                   }
  deriving (Show, Eq)

data Door = Door { _doorMapName :: String
                 , _doorId :: String
                 }
  deriving (Show, Eq)
doorMapName = Lens { view = \(Door name _) -> name
                   , set  = \name (Door _ ident) -> Door name ident
                   }
doorId = Lens { view = \(Door _ ident) -> ident
              , set  = \ident (Door name _) -> Door name ident
              }


data GridBead = Wall
              | Empty
              | LightBead Light
              | DoorBead Door
  deriving (Show, Eq)

type Color = (Byte, Byte, Byte)
type DistanceX = Int
data BeadColor = WallColor DistanceX
               | DoorColor DistanceX
               | EmptyColor
               | PlayerColor
               | LightColor Color
  deriving (Show, Eq)

data Facing = Positive | Negative
  deriving (Show, Eq)

type Position = (GridX, GridY, GridZ)
posX = Lens { view = \(x, _, _) -> x
            , set  = \x (_, y, z) -> (x, y, z)
            }
posY = Lens { view = \(_, y, _) -> y
            , set  = \y (x, _, z) -> (x, y, z)
            }
posZ = Lens { view = \(_, _, z) -> z
            , set  = \z (x, y, _) -> (x, y, z)
            }
