module GameLogic.Player ( Player
                        , playerFacing
                        , playerPosition
                        , playerApplyMove
                        , playerChangeDirection
                        ) where

import Prelude
import GameLogic.Move ( Move
                      , applyMove
                      )
import GameLogic.Types ( Facing(..)
                       )
import GameLogic.Types.Player
    ( Player
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
    changePosition = applyMove move (player^.playerFacing)
