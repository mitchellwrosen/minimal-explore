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

data Player = Player { _position :: (GridX, GridY, GridZ)
                     , _facing :: Facing
                     }
  deriving (Show, Eq)

playerGetFacing :: Player -> Facing
playerGetFacing = _facing

playerChangeDirection :: Player -> Player
playerChangeDirection player = player { _facing = oppositeFacing facing }
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive
    facing = playerGetFacing player

playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = player { _position = position }
  where
    position = move (_facing player) (_position player)

playerGetPosition :: Player -> (GridX, GridY, GridZ)
playerGetPosition = _position
