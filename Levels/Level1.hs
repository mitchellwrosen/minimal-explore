module Levels.Level1 (gameState) where

import GameLogic.State ( GameState(..) )
import GameLogic.Types ( GridBead(..) )
import GameLogic.Grid ( Grid(..) )
import GameLogic.Player ( Player(..)
                        , Facing(..)
                        )

{-
 -grid :: Grid GridBead
 -grid = [ [ [ Wall, Wall, Wall ]
 -         , [ Wall, Empty, Wall ]
 -         , [ Wall, Wall, Wall ]
 -         ]
 -
 -       , [ [ Empty, Empty, Empty ]
 -         , [ Empty, Empty, Empty ]
 -         , [ Empty, Wall, Empty ]
 -         ]
 -
 -       , [ [ Empty, Empty, Empty ]
 -         , [ Wall, Empty, Empty ]
 -         , [ Wall, Wall, Empty ]
 -         ]
 -       ]
 -}

grid :: Grid GridBead
grid = [ [ [ Empty, Empty, Wall, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Wall, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Wall, Empty, Empty ]
         , [ Empty, Empty, Wall, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Wall, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Wall, Empty, Empty ]
         , [ Empty, Empty, Wall, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         ]

       , [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Empty, Empty, Wall, Empty, Empty, Empty ]
         , [ Empty, Empty, Wall, Empty, Wall, Empty, Empty ]
         , [ Empty, Wall, Empty, Empty, Empty, Wall, Empty ]
         , [ Empty, Wall, Wall, Wall, Wall, Wall, Empty ]
         , [ Empty, Wall, Empty, Empty, Empty, Wall, Empty ]
         , [ Empty, Wall, Empty, Empty, Empty, Wall, Empty ]
         , [ Empty, Wall, Empty, Empty, Empty, Wall, Empty ]
         ]

       , [ [ Empty, Empty, Wall, Wall, Empty, Empty, Empty ]
         , [ Empty, Wall, Empty, Empty, Wall, Empty, Empty ]
         , [ Empty, Wall, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Wall, Empty, Empty, Empty, Empty, Empty, Empty ]
         , [ Empty, Wall, Empty, Empty, Wall, Empty, Empty ]
         , [ Empty, Wall, Empty, Empty, Wall, Empty, Empty ]
         , [ Empty, Empty, Wall, Wall, Empty, Empty, Empty ]
         ]
       ]

player :: Player
player = Player (0, 1, 1) Positive

gameState :: GameState
gameState = GameState player grid
