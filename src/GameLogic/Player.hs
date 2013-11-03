module GameLogic.Player ( Player
                        , playerFacing
                        , playerPosition
                        , playerApplyMove
                        , playerChangeDirection
                        ) where

import Prelude
import GameLogic.Move ( Move
                      )
import GameLogic.Types ( Facing(..)
                       , Player
                       , playerFacing
                       , playerPosition
                       )
import Control.Lens ( (^.)
                    , over
                    )

playerChangeDirection :: Player -> Player
playerChangeDirection = over playerFacing oppositeFacing
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive

playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = over playerPosition changePosition player
  where
    changePosition = move (player^.playerFacing)
