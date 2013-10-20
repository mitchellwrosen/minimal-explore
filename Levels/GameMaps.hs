module Levels.GameMaps (gameMaps) where

import Prelude ( String
               )
import GameLogic.GameMap ( GameMap(..)
                         )
import qualified Levels.Level1
import qualified Levels.Level2

assocTuple :: GameMap -> (String, GameMap)
assocTuple gameMap = (gameMapName gameMap, gameMap)

gameMaps :: [(String, GameMap)]
gameMaps = [ assocTuple Levels.Level1.gameMap
           , assocTuple Levels.Level2.gameMap
           ]
