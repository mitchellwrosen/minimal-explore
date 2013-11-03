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

import Prelude 
import GameLogic.Grid ( Grid
                      , gridElems
                      , gridSet
                      )
import GameLogic.Move ( Move
                      )
import GameLogic.Types ( GridBead(..)
                       , Byte
                       , doorMapName
                       , doorId
                       , Position
                       , Facing(..)
                       , MapLight
                       , GameMap(..)
                       , gameMapGrid
                       , gameMapAmbientLight
                       , gameMapLights
                       , gameMapDoors
                       , gameMapName
                       )

import Data.Util.Maybe ( fromMaybe )
import Data.Util.List ( findFirst
                      , filterMap
                      )
import Control.Lens ( (^.)
                    , over
                    )

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
getGameMapFromDoor _ _ = error "Should be a Door Bead"

getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> Position
getMatchingDoorPosition fromMap toMap (DoorBead door) =
    snd $ findFirst (matchingDoor . fst) (toMap^.gameMapDoors)
  where
    matchingDoor d =
        (d^.doorMapName == fromMap^.gameMapName) && (d^.doorId == door^.doorId)
getMatchingDoorPosition _ _ _ = error "Should be a Door Bead"

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
