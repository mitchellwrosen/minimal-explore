module GameLogic.Types.Player ( makePlayer
                              , playerPosition
                              , playerFacing
                              , Player
                              ) where

import Prelude
import GameLogic.Types ( Position
                       , Facing
                       )
import Control.Lens ( Lens(..) )

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
