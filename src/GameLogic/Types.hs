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

data Player = Player { _playerPosition :: Position
                     , _playerFacing :: Facing
                     }
  deriving (Show, Eq)
makePlayer :: Position -> Facing -> Player
makePlayer = Player

playerPosition :: Lens Player Position
playerPosition = Lens { view = _playerPosition
                      , set = \pos player -> player { _playerPosition = pos }
                      }

playerFacing :: Lens Player Facing
playerFacing = Lens { view = _playerFacing
                    , set = \fac player -> player { _playerFacing = fac }
                    }

type Grid a = [[[a]]]
type MapDoor  = (Door, Position)
type MapLight = (Light, Position)
data GameMap = GameMap { _gameMapGrid :: Grid GridBead
                       , _gameMapName :: String
                       , _gameMapDoors :: [MapDoor]
                       , _gameMapLights :: [MapLight]
                       , _gameMapAmbientLight :: Byte
                       }
  deriving (Eq, Show)
gameMapGrid :: Lens GameMap (Grid GridBead)
gameMapGrid = Lens { view = _gameMapGrid
                   , set  = \val gameState  -> gameState { _gameMapGrid = val }
                   }
gameMapName :: Lens GameMap String
gameMapName = Lens { view = _gameMapName
                   , set  = \val gameState  -> gameState { _gameMapName = val }
                   }
gameMapDoors :: Lens GameMap [MapDoor]
gameMapDoors = Lens { view = _gameMapDoors
                    , set  = \val gameState  -> gameState { _gameMapDoors = val }
                    }
gameMapLights :: Lens GameMap [MapLight]
gameMapLights = Lens { view = _gameMapLights
                     , set  = \val gameState  -> gameState { _gameMapLights = val }
                     }
gameMapAmbientLight :: Lens GameMap Byte
gameMapAmbientLight = Lens
                   { view = _gameMapAmbientLight
                   , set  = \val gameState  -> gameState { _gameMapAmbientLight = val }
                   }

data GameState = GameState { _gameStatePlayer :: Player
                           , _gameStateGameMap :: GameMap
                           , _gameStateGameMaps :: [(String, GameMap)]
                           }
  deriving (Show, Eq)
gameStatePlayer :: Lens GameState Player
gameStatePlayer = Lens { view = _gameStatePlayer
                       , set = \player gameState -> gameState { _gameStatePlayer = player }
                       }
gameStateGameMap :: Lens GameState GameMap
gameStateGameMap = Lens { view = _gameStateGameMap
                        , set = \gameMap gameState -> gameState { _gameStateGameMap = gameMap }
                        }
gameStateGameMaps :: Lens GameState [(String, GameMap)]
gameStateGameMaps = Lens { view = _gameStateGameMaps
                         , set = \gameMaps gameState -> gameState { _gameStateGameMaps = gameMaps }
                         }
makeGameState :: Player -> GameMap -> [(String, GameMap)] -> GameState
makeGameState = GameState
