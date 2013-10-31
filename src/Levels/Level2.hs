module Levels.Level2 (gameMap) where

import Prelude ( ($) )

import GameLogic.GameMap ( GameMap(..)
                         , makeGameMap
                         )
import GameLogic.Types ( GridBead(..)
                       , Light(..)
                       , Door(..)
                       )
import GameLogic.Grid ( Grid(..) )

grid :: Grid GridBead
grid = [ [ [ Wall, Empty, Wall, Empty, Wall, Empty, Wall, Empty, Wall ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall ]
         , [ Wall, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Wall ]
         ]
       , [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
         , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
         , [ Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall ]
         , [ Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall ]
         , [ Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall ]
         , [ Wall, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Wall ]
         ]
       , [ [ Empty, Wall, Empty, Empty, Empty, Empty, Empty, Wall, Empty ]
         , [ Wall, Empty, Wall, Wall, Empty, Wall, Wall, Empty, Wall ]
         , [ Empty, Wall, Empty, Wall, Wall, Wall, Empty, Wall, Empty ]
         , [ Empty, Wall, Empty, Wall, Empty, Wall, Empty, Wall, DoorBead $ Door "level1" "a" (64, 64, 64)]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, LightBead $ Light 3 (255, 255, 255), Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty ]
         ]
       ]

gameMap :: GameMap
gameMap = makeGameMap grid "level2" 192