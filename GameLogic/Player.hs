module GameLogic.Player ( Player(..)
                        , playerGetFacing
                        , playerGetPosition
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
                       )

-- TODO(R): _position => playerPosition
-- TODO(R): _facing => playerFacing
data Player = Player { _position :: (GridX, GridY, GridZ)
                     , _facing :: Facing
                     }
  deriving (Show, Eq)

-- TODO(R): remove playerGetFacing
playerGetFacing :: Player -> Facing
playerGetFacing = _facing

-- TODO(R): remove playerGetPosition
playerGetPosition :: Player -> (GridX, GridY, GridZ)
playerGetPosition = _position

-- TODO(R): lenses
playerChangeDirection :: Player -> Player
playerChangeDirection player = player { _facing = oppositeFacing facing }
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive
    facing = playerGetFacing player

-- TODO(R): lenses
playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = player { _position = position }
  where
    position = move (_facing player) (_position player)
