module GameLogic.State ( GameState(..)
                       , isValidPlayerPosition
                       , leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       ) where

import Prelude ( Show
               , Eq
               , Bool(..)
               , Maybe(..)
               , (==)
               )
import GameLogic.Player ( Player(..)
                        , playerGetPosition
                        , playerMoveLeft
                        , playerMoveRight
                        , playerMoveUp
                        , playerMoveDown
                        , playerMoveForward
                        , playerChangeDirection
                        )
import GameLogic.Grid ( Grid(..)
                      , gridGet
                      )
import GameLogic.Types ( GridBead(..)
                       )

data GameState = GameState { _player :: Player
                           , _grid :: Grid GridBead
                           }
  deriving (Show, Eq)

isValidPlayerPosition :: GameState -> Bool
isValidPlayerPosition gameState =
    case gridGet grid x y z of
        Just something -> something == Empty
        _ -> False
  where grid = _grid gameState
        player = _player gameState
        (x, y, z) = playerGetPosition player

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

forwardButtonPressed :: GameState -> GameState
forwardButtonPressed gameState@(GameState player grid) =
    if isValidPlayerPosition gameState'
    then gameState'
    else gameState
  where
    gameState' = GameState (playerMoveForward player) grid

reverseButtonPressed :: GameState -> GameState
reverseButtonPressed gameState@(GameState player grid) = gameState'
  where
    gameState' = GameState (playerChangeDirection player) grid
