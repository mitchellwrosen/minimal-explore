module Levels.Level1 (gameMap) where

import Prelude
    ( ($) )

import GameLogic.GameMap
    ( GameMap
    , makeGameMap
    )
import GameLogic.Types
    ( GridBead(..)
    , Light(..)
    , Door(..)
    , Gate(..)
    , makeColor
    )
import GameLogic.Grid
    ( Grid )

grid :: Grid GridBead
grid = [ [ [ Wall, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Wall ]
         , [ Empty, Empty, LightBead $ Light 3 $ makeColor (255, 0, 0), Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, DoorBead $ Door "level2" "a" $ makeColor (192, 192, 64), Empty, Empty, Empty, Wall ]
         , [ Empty, Empty, Empty, Empty, LightBead $ Light 2 $ makeColor (0, 0, 255), Empty, LightBead $ Light 1 $ makeColor (0, 255, 0), Empty, Empty ]
         , [ Wall, Empty, Empty, GateBead $ Gate $ makeColor (192, 64, 64), Empty, Empty, Empty, Empty, Wall ]
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
         , [ Empty, Wall, Empty, Wall, Empty, Wall, Empty, Wall, Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty ]
         ]
       ]

gameMap :: GameMap
gameMap = makeGameMap grid "level1" 64
