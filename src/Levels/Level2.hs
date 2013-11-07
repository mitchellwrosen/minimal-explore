module Levels.Level2 (gameMap) where

import Prelude

import GameLogic.GameMap
    ( GameMap
    , makeGameMap
    )
import GameLogic.Types
    ( GridBead(..)
    , Light(..)
    , Door(..)
    , makeColor
    )
import GameLogic.Grid
    ( Grid )

grid :: Grid GridBead
grid = [ [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         ]
       , [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Wall, TextBead "Hello" $ makeColor (255, 255, 255), Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Wall, TextBead "World" $ makeColor (255, 255, 255), Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, DoorBead $ Door "level1" "a" $ makeColor (255, 255, 255)]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, LightBead $ Light 3 $ makeColor (255, 255, 255), Empty ]
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
