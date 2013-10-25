module Levels.GameMaps (gameMaps) where

import Prelude ( String
               )
import GameLogic.GameMap ( GameMap
                         , gameMapName
                         )
import qualified Levels.Level1
import qualified Levels.Level2

import Control.Lens ( (^.) )

assocTuple :: GameMap -> (String, GameMap)
assocTuple gameMap = (gameMap ^. gameMapName, gameMap)

gameMaps :: [(String, GameMap)]
gameMaps = [ assocTuple Levels.Level1.gameMap
           , assocTuple Levels.Level2.gameMap
           ]
