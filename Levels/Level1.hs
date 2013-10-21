module Levels.Level1 (gameMap) where

import GameLogic.GameMap ( GameMap(..)
                         , makeGameMap
                         )
import GameLogic.Types ( GridBead(..) )
import GameLogic.Grid ( Grid(..) )

grid :: Grid GridBead
grid = [ [ [ Wall, Empty, Wall, Empty, Wall, Empty, Wall, Empty, Wall ]
         , [ Empty, Light 3 (255, 255, 0), Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, Door "level2" "a", Empty, Empty, Empty, Wall ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Light 1 (0, 255, 0), Empty ]
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
         , [ Empty, Wall, Empty, Wall, Empty, Wall, Empty, Wall, Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty ]
         ]
       ]

gameMap :: GameMap
gameMap = makeGameMap grid "level1" 64
