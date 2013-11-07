module Levels.Level2 (gameMap) where

import Prelude

import GameLogic.GameMap ( GameMap
                         , makeGameMap
                         )
import GameLogic.Types ( GridBead(..)
                       , Light(..)
                       , Door(..)
                       )
import GameLogic.Grid ( Grid )

grid :: Grid GridBead
grid = [ [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         ]
       , [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Wall, TextBead "Hello" (255, 255, 255), Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Wall, TextBead "World" (255, 255, 255), Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, DoorBead $ Door "level1" "a" (255, 255, 255)]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, LightBead $ Light 3 (255, 255, 255), Empty ]
         ]
       , [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         ]
       ]

gameMap :: GameMap
gameMap = makeGameMap grid "level2" 192
