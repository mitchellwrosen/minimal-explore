module GameLogic.GameMap ( GameMap(..)
                         , getGameMapFromDoor
                         , getMatchingDoor
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
               , error
               , id
               , (==)
               , (++)
               , ($)
               )

import GameLogic.Grid ( Grid(..)
                      , gridElems
                      )
import GameLogic.Types ( GridBead(..)
                       )

data GameMap = GameMap { gameMapGrid :: Grid GridBead
                       , gameMapName :: String
                       , gameMapDoors :: [GridBead]
                       }
  deriving (Eq, Show)

getGameMapFromDoor :: [(String, GameMap)] -> GridBead -> GameMap
getGameMapFromDoor gameMaps (Door roomName _) =
    maybe (error $ "Bad RoomName " ++ roomName) id $ lookup roomName gameMaps


getMatchingDoor :: GameMap -> GameMap -> GridBead -> GridBead
getMatchingDoor fromMap toMap (Door name ident) =
    findFirst (== Door (gameMapName fromMap) ident) (gameMapDoors toMap)
  where
    findFirst :: (a -> Bool) -> [a] -> a
    findFirst filt list = head $ filter filt list

makeGameMap :: Grid GridBead -> String -> GameMap
makeGameMap grid name = GameMap grid name doors
  where
    filterFunc (Door _ _) = True
    filterFunc _ = False
    doors = filter filterFunc $ gridElems grid
