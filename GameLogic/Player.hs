module GameLogic.Player ( Player(..)
                        {-, playerFacing-}
                        {-, playerPosition-}
                        , playerApplyMove
                        , playerChangeDirection
                        ) where

import Prelude ( Show
               , Eq
               , (+)
               , (-)
               , (==)
               )
import GameLogic.Move ( Move(..)
                      , Facing(..)
                      )
import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , Position
                       )

data Player = Player { _playerPosition :: Position
                     , _playerFacing :: Facing
                     }
  deriving (Show, Eq)

-- TODO(R): lenses
playerChangeDirection :: Player -> Player
playerChangeDirection player = player { _playerFacing = oppositeFacing facing }
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive
    facing = _playerFacing player

-- TODO(R): lenses
playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = player { _playerPosition = position }
  where
    position = move (_playerFacing player) (_playerPosition player)
