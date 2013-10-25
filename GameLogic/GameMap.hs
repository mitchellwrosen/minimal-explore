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
               , ($)
               , (.)
               )

import GameLogic.Grid ( Grid(..)
                      , gridElems
                      , gridSet
                      )
import GameLogic.Move ( Move(..)
                      , Facing(..)
                      )
import GameLogic.Types ( GridBead(..)
                       , GridX
                       , GridY
                       , GridZ
                       , Byte
                       , Light(..)
                       , Door(..)
                       , Position(..)
                       )

import Data.Util.Maybe ( fromMaybe )
import Data.Util.List ( findFirst
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

gameMapGrid = Lens { view = \gameState -> _gameMapGrid gameState
                   , set  = \val gameState  -> gameState { _gameMapGrid = val }
                   }
gameMapName = Lens { view = \gameState -> _gameMapName gameState
                   , set  = \val gameState  -> gameState { _gameMapName = val }
                   }
gameMapDoors = Lens { view = \gameState -> _gameMapDoors gameState
                   , set  = \val gameState  -> gameState { _gameMapDoors = val }
                   }
gameMapLights = Lens { view = \gameState -> _gameMapLights gameState
                   , set  = \val gameState  -> gameState { _gameMapLights = val }
                   }
gameMapAmbientLight = Lens { view = \gameState -> _gameMapAmbientLight gameState
                   , set  = \val gameState  -> gameState { _gameMapAmbientLight = val }
                   }

gameMapApplyMoveLight :: GameMap -> MapLight -> Facing -> Move -> GameMap
gameMapApplyMoveLight gameMap light facing move =
    over gameMapLights (map moveLight) gameMap
  where
    moveLight l
        | l == light = applyMove light
        | otherwise = l

    applyMove (l, pos) = (l, move facing  pos)

getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (DoorBead (Door roomName _)) =
    fromMaybe (error $ "Bad RoomName " ++ roomName) $ lookup roomName gameMaps

-- TODO(R): compare on door ident/map
getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> Position
getMatchingDoorPosition fromMap toMap (DoorBead (Door name ident)) =
    snd $ findFirst ((== Door (fromMap ^. gameMapName) ident) . fst) (toMap ^. gameMapDoors)

makeGameMap :: Grid GridBead -> String -> Byte -> GameMap
makeGameMap grid name ambientLight =
    GameMap grid' name doors lights ambientLight
  where
    lights = foldr lightFold [] (gridElems grid)
      where
        -- TODO(R): LightBead could have record syntax
        lightFold (LightBead light, pos) xs = (light, pos):xs
        lightFold _ xs = xs

    grid' = removeLightBeads
    removeLightBeads = foldr removeLightBead grid lights
      where
        removeLightBead (_, pos) grd = gridSet grd pos Empty

    doors = foldr doorFold [] (gridElems grid)
      where
        -- TODO(R): DoorBead could have record syntax
        doorFold (DoorBead door, pos) xs = (door, pos):xs
        doorFold _ xs = xs
