module GameLogic.Player ( Player
                        , makePlayer
                        , playerFacing
                        , playerPosition
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
import Control.Lens ( (^.)
                    , over
                    , Lens(..)
                    )

data Player = Player { _playerPosition :: Position
                     , _playerFacing :: Facing
                     }
  deriving (Show, Eq)

makePlayer = Player
playerPosition = Lens { view = \(Player pos _) -> pos
                      , set  = \pos (Player _ fac) -> Player pos fac
                      }
playerFacing = Lens { view = \(Player _ fac) -> fac
                    , set  = \fac (Player pos _) -> Player pos fac
                    }

playerChangeDirection :: Player -> Player
playerChangeDirection player = over playerFacing oppositeFacing player
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive

playerApplyMove :: Player -> Move -> Player
playerApplyMove player move = over playerPosition changePosition player
  where
    changePosition = move (player ^. playerFacing)
