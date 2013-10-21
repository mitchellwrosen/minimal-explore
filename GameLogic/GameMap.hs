module GameLogic.GameMap ( GameMap(..)
                         , getGameMapFromDoor
                         , getMatchingDoorPosition
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
               , error
               , id
               , fst
               , snd
               , map
               , (==)
               , (++)
               , ($)
               , (.)
               )

import GameLogic.Grid ( Grid(..)
                      , gridElems
                      )
import GameLogic.Types ( GridBead(..)
                       , GridX
                       , GridY
                       , GridZ
                       )

data GameMap = GameMap { gameMapGrid :: Grid GridBead
                       , gameMapName :: String
                       , gameMapDoors :: [(GridBead, (GridX, GridY, GridZ))]
                       , gameMapAmbientLight :: Int
                       }
  deriving (Eq, Show)

getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (Door roomName _) =
    maybe (error $ "Bad RoomName " ++ roomName) id $ lookup roomName gameMaps


getMatchingDoorPosition :: GameMap -> GameMap -> GridBead -> (GridX, GridY, GridZ)
getMatchingDoorPosition fromMap toMap (Door name ident) =
    snd $ findFirst ((== Door (gameMapName fromMap) ident) . fst) (gameMapDoors toMap)
  where
    findFirst :: (a -> Bool) -> [a] -> a
    findFirst filt list = head $ filter filt list

makeGameMap :: Grid GridBead -> String -> Int -> GameMap
makeGameMap grid name ambientLight = GameMap grid name doors ambientLight
  where
    filterFunc ((Door _ _), _) = True
    filterFunc _ = False
    doors = filter filterFunc $ gridElems grid
