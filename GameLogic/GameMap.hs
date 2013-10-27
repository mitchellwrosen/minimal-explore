module GameLogic.GameMap ( GameMap
                         , gameMapGrid
                         , gameMapName
                         , gameMapLights
                         , gameMapDoors
                         , gameMapAmbientLight
                         , getGameMapFromDoor
                         , getMatchingDoorPosition
                         , gameMapApplyMoveLight
                         , makeGameMap
                         ) where

import Prelude ( String
               , Eq
               , Show
               , Maybe(..)
               , Bool(..)
               , lookup
               , head
               , maybe
               , filter
               , foldr
               , error
               , id
               , fst
               , snd
               , otherwise
               , map
               , (==)
               , (++)
               , (&&)
               , ($)
               , (.)
               )

import GameLogic.Grid ( Grid(..)
                      , gridElems
                      , gridSet
                      )
import GameLogic.Move ( Move(..)
                      )
import GameLogic.Types ( GridBead(..)
                       , GridX
                       , GridY
                       , GridZ
                       , Byte
                       , Door
                       , doorMapName
                       , doorId
                       , Light(..)
                       , Position(..)
                       , Facing(..)
                       )

import Data.Util.Maybe ( fromMaybe )
import Data.Util.List ( findFirst
                      , filterMap
                      , replace
                      )
import Control.Lens ( (^.)
                    , over
                    , Lens(..)
                    )

type MapDoor  = (Door, Position)
type MapLight = (Light, Position)
data GameMap = GameMap { _gameMapGrid :: Grid GridBead
                       , _gameMapName :: String
                       , _gameMapDoors :: [MapDoor]
                       , _gameMapLights :: [MapLight]
                       , _gameMapAmbientLight :: Byte
                       }
  deriving (Eq, Show)
gameMapGrid = Lens { view = _gameMapGrid
                   , set  = \val gameState  -> gameState { _gameMapGrid = val }
                   }
gameMapName = Lens { view = _gameMapName
                   , set  = \val gameState  -> gameState { _gameMapName = val }
                   }
gameMapDoors = Lens { view = _gameMapDoors
                    , set  = \val gameState  -> gameState { _gameMapDoors = val }
                    }
gameMapLights = Lens { view = _gameMapLights
                     , set  = \val gameState  -> gameState { _gameMapLights = val }
                     }
gameMapAmbientLight = Lens
                   { view = _gameMapAmbientLight
                   , set  = \val gameState  -> gameState { _gameMapAmbientLight = val }
                   }

gameMapApplyMoveLight :: GameMap -> MapLight -> Facing -> Move -> GameMap
gameMapApplyMoveLight gameMap light facing move =
    over gameMapLights (map moveLight) gameMap
  where
    moveLight lite@(l, pos)
        | lite == light = (l, move facing pos)
        | otherwise = lite

getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (DoorBead door) =
    fromMaybe (error $ "Bad RoomName " ++ roomName) $ lookup roomName gameMaps
  where
    roomName = door^.doorMapName

getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> Position
getMatchingDoorPosition fromMap toMap (DoorBead door) =
    snd $ findFirst (matchingDoor . fst) (toMap^.gameMapDoors)
  where
    matchingDoor d =
        (d^.doorMapName == fromMap^.gameMapName) && (d^.doorId == door^.doorId)

makeGameMap :: Grid GridBead -> String -> Byte -> GameMap
makeGameMap grid name ambientLight =
    GameMap grid' name doors lights ambientLight
  where
    lights = filterMap getMapLight $ gridElems grid
      where
        getMapLight (LightBead light, pos) = Just (light, pos)
        getMapLight _ = Nothing

    doors = filterMap getMapDoor $ gridElems grid
      where
        getMapDoor (DoorBead door, pos) = Just (door, pos)
        getMapDoor _ = Nothing

    grid' = foldr removeLightBead grid lights
      where
        removeLightBead (_, pos) grd = gridSet grd pos Empty
