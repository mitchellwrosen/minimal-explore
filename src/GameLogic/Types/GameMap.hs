module GameLogic.Types.GameMap ( gameMapGrid
                               , gameMapName
                               , gameMapDoors
                               , gameMapLights
                               , gameMapAmbientLight
                               , MapDoor
                               , MapLight
                               , GameMap(..)
                               ) where

import Prelude
import GameLogic.Types ( Door
                       , Position
                       , Light
                       , Grid
                       , Byte
                       , GridBead
                       )
import Control.Lens ( Lens(..) )

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
