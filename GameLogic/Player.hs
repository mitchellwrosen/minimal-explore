module GameLogic.Player where

import Main.Types (GridX, GridY, GridZ)

data Facing = Positive | Negative
  deriving (Show, Eq)

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

playerMoveForward :: Player -> Player
playerMoveForward player = player { _position = (x+1, y, z) }
  where
    (x, y, z) = playerGetPosition player

playerMoveUp :: Player -> Player
playerMoveUp player = player { _position = (x, y+1, z) }
  where
    (x, y, z) = playerGetPosition player

playerMoveDown :: Player -> Player
playerMoveDown player = player { _position = (x, y-1, z) }
  where
    (x, y, z) = playerGetPosition player

playerMoveLeft :: Player -> Player
playerMoveLeft player = player { _position = (x, y, delta z) }
  where
    delta
        | playerGetFacing player == Positive = (+) (-1)
        | playerGetFacing player == Negative = (+) 1
    (x, y, z) = playerGetPosition player

playerMoveRight :: Player -> Player
playerMoveRight player = player { _position = (x, y, delta z) }
  where
    delta
        | playerGetFacing player == Positive = (+) 1
        | playerGetFacing player == Negative = (+) (-1)
    (x, y, z) = playerGetPosition player

playerGetPosition :: Player -> (GridX, GridY, GridZ)
playerGetPosition player = _position player