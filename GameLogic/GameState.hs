module GameLogic.GameState where

import GameLogic.Player
import GameLogic.Grid
import Main.Types

data GameState = GameState { _player :: Player
                           , _grid :: Grid GridBead
                           }
  deriving (Show, Eq)

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f l = zipWith f [0..] l

isValidPlayerPosition :: GameState -> Bool
isValidPlayerPosition gameState =
    gridGet grid x y z == Just Empty
  where grid = _grid gameState
        player = _player gameState
        (x, y, z) = (playerGetPosition player)

leftButtonPressed :: GameState -> GameState
leftButtonPressed gameState@(GameState player grid) =
    if isValidPlayerPosition gameState'
    then gameState'
    else gameState
  where
    gameState' = GameState (playerMoveLeft player) grid

rightButtonPressed :: GameState -> GameState
rightButtonPressed gameState@(GameState player grid) =
    if isValidPlayerPosition gameState'
    then gameState'
    else gameState
  where
    gameState' = GameState (playerMoveRight player) grid

upButtonPressed :: GameState -> GameState
upButtonPressed gameState@(GameState player grid) =
    if isValidPlayerPosition gameState'
    then gameState'
    else gameState
  where
    gameState' = GameState (playerMoveUp player) grid

downButtonPressed :: GameState -> GameState
downButtonPressed gameState@(GameState player grid) =
    if isValidPlayerPosition gameState'
    then gameState'
    else gameState
  where
    gameState' = GameState (playerMoveDown player) grid
