module GameLogic.Types where

import Prelude
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

data Gate = Gate { gateColor :: Color
                 }
  deriving (Show, Eq)

data Door = Door { _doorMapName :: String
                 , _doorId :: String
                 , doorColor :: Color
                 }
  deriving (Show, Eq)
doorMapName :: Lens Door String
doorMapName = Lens { view = _doorMapName
                   , set  = \name door -> door { _doorMapName = name }
                   }
doorId :: Lens Door String
doorId = Lens { view = _doorId
              , set  = \ident door -> door { _doorId = ident }
              }


data GridBead = Wall
              | Empty
              | TextBead String Color
              | LightBead Light
              | DoorBead Door
              | GateBead Gate
  deriving (Show, Eq)

isEmpty :: GridBead -> Bool
isEmpty Empty = True
isEmpty (TextBead _ _) = True
isEmpty _ = False

data Color = Color (Byte, Byte, Byte)
  deriving (Show, Eq)

makeColor :: (Byte, Byte, Byte) -> Color
makeColor (r, g, b) = Color (clamp r, clamp g, clamp b)
  where clamp val = min (max 0 val) 255

colorR :: Lens Color Byte
colorR = Lens
    { view = \(Color (r, _, _)) -> r
    , set = \r (Color (_, g, b)) -> makeColor (r, g, b)
    }

colorG :: Lens Color Byte
colorG = Lens
    { view = \(Color (_, g, _)) -> g
    , set = \g (Color (r, _, b)) -> makeColor (r, g, b)
    }

colorB :: Lens Color Byte
colorB = Lens
    { view = \(Color (_, _, b)) -> b
    , set = \b (Color (r, g, _)) -> makeColor (r, g, b)
    }

type DistanceX = Int
data BeadColor = WallColor DistanceX
               | DoorColor DistanceX
               | GateColor DistanceX
               | EmptyColor
               | PlayerColor
               | LightColor Color
  deriving (Show, Eq)

data Facing = Positive | Negative
  deriving (Show, Eq)

type Position = (GridX, GridY, GridZ)
posX :: Lens Position GridX
posX = Lens { view = \(x, _, _) -> x
            , set  = \x (_, y, z) -> (x, y, z)
            }
posY :: Lens Position GridY
posY = Lens { view = \(_, y, _) -> y
            , set  = \y (x, _, z) -> (x, y, z)
            }
posZ :: Lens Position GridZ
posZ = Lens { view = \(_, _, z) -> z
            , set  = \z (x, y, _) -> (x, y, z)
            }

type Grid a = [[[a]]]
