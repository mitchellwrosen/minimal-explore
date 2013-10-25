module GameLogic.GameMap ( GameMap(..)
                         , getGameMapFromDoor
                         , getMatchingDoorPosition
                         , gameMapApplyMoveLight
                         , makeGameMap
                         ) where

import Prelude ( String
               , Int
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
                      , replace
                      )
import GameLogic.Move ( Move(..)
                      , Facing(..)
                      , Position(..)
                      )
import GameLogic.Types ( GridBead(..)
                       , GridX
                       , GridY
                       , GridZ
                       , Light(..)
                       , Door(..)
                       )

-- TODO(R): Type for 0-255 values
-- TODO(R): Only export the accessor functions
-- TODO(R): Introduce types: MapDoor MapLight
data GameMap = GameMap { gameMapGrid :: Grid GridBead
                       , gameMapName :: String
                       , gameMapDoors :: [(Door, (GridX, GridY, GridZ))]
                       , gameMapLights :: [(Light, (GridX, GridY, GridZ))]
                       , gameMapAmbientLight :: Int
                       }
  deriving (Eq, Show)

gameMapApplyMoveLight :: GameMap -> (Light, Position) -> Facing -> Move -> GameMap
gameMapApplyMoveLight gameMap light facing move =
    gameMap { gameMapLights = lights' }
  where
    lights' = map moveLight $ gameMapLights gameMap
    moveLight l
        | l == light = applyMove light
        | otherwise = l

    applyMove (l, pos) = (l, move facing  pos)

getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (DoorBead (Door roomName _)) =
    maybe (error $ "Bad RoomName " ++ roomName) id $ lookup roomName gameMaps

getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> (GridX, GridY, GridZ)
getMatchingDoorPosition fromMap toMap (DoorBead (Door name ident)) =
    snd $ findFirst ((== Door (gameMapName fromMap) ident) . fst) (gameMapDoors toMap)
  where
    findFirst :: (a -> Bool) -> [a] -> a
    findFirst filt list = head $ filter filt list

makeGameMap :: Grid GridBead -> String -> Int -> GameMap
makeGameMap grid name ambientLight =
    GameMap grid' name doors lights ambientLight
  where
    lights = foldr lightFold [] (gridElems grid)
      where
        lightFold (LightBead light, pos) xs = (light, pos):xs
        lightFold _ xs = xs

    grid' = removeLightBeads
    removeLightBeads = foldr removeLightBead grid lights
      where
        removeLightBead (_, (x, y, z)) grd = gridSet grd x y z Empty

    doors = foldr doorFold [] (gridElems grid)
      where
        doorFold (DoorBead door, pos) xs = (door, pos):xs
        doorFold _ xs = xs
