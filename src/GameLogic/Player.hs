module GameLogic.Player ( Player
                        , makePlayer
                        , playerFacing
                        , playerPosition
                        , playerApplyMove
                        , playerChangeDirection
                        ) where

import Prelude
import GameLogic.Move ( Move
                      )
import GameLogic.Types ( Position
                       , Facing(..)
                       )
import Control.Lens ( (^.)
                    , over
                    , Lens(..)
                    )

data Player = Player { _playerPosition :: Position
                     , _playerFacing :: Facing
                     }
  deriving (Show, Eq)
makePlayer :: Position -> Facing -> Player
makePlayer = Player

playerPosition :: Lens Player Position
playerPosition = Lens { view = _playerPosition
                      , set = \pos player -> player { _playerPosition = pos }
                      }

playerFacing :: Lens Player Facing
playerFacing = Lens { view = _playerFacing
                    , set = \fac player -> player { _playerFacing = fac }
                    }

playerChangeDirection :: Player -> Player
playerChangeDirection = over playerFacing oppositeFacing
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive

playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = over playerPosition changePosition player
  where
    changePosition = move (player^.playerFacing)
