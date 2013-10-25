module GameLogic.GameMap ( GameMap(..)
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
                      , replace
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

type MapDoor  = (Door, Position)
type MapLight = (Light, Position)
data GameMap = GameMap { gameMapGrid :: Grid GridBead
                       , gameMapName :: String
                       , gameMapDoors :: [MapDoor]
                       , gameMapLights :: [MapLight]
                       , gameMapAmbientLight :: Byte
                       }
  deriving (Eq, Show)

gameMapApplyMoveLight :: GameMap -> MapLight -> Facing -> Move -> GameMap
gameMapApplyMoveLight gameMap light facing move =
    -- TODO(R): Lens
    gameMap { gameMapLights = lights' }
  where
    lights' = map moveLight $ gameMapLights gameMap
    moveLight l
        | l == light = applyMove light
        | otherwise = l

    applyMove (l, pos) = (l, move facing  pos)

-- TODO(R): fromMaybe
getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (DoorBead (Door roomName _)) =
    maybe (error $ "Bad RoomName " ++ roomName) id $ lookup roomName gameMaps

-- TODO(R): compare on door ident/map
getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> Position
getMatchingDoorPosition fromMap toMap (DoorBead (Door name ident)) =
    snd $ findFirst ((== Door (gameMapName fromMap) ident) . fst) (gameMapDoors toMap)
  where
    -- TODO(R): helper function
    findFirst :: (a -> Bool) -> [a] -> a
    findFirst filt list = head $ filter filt list

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
        removeLightBead (_, (x, y, z)) grd = gridSet grd x y z Empty

    doors = foldr doorFold [] (gridElems grid)
      where
        -- TODO(R): DoorBead could have record syntax
        doorFold (DoorBead door, pos) xs = (door, pos):xs
        doorFold _ xs = xs
