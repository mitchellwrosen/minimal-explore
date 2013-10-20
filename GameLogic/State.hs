module GameLogic.State ( GameState(..)
                       , gsPlayer
                       , gsGrid
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
                        , pPosition
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

import Control.Lens

data GameState = GameState { _gsPlayer :: Player
                           , _gsGrid :: Grid GridBead
                           }
  deriving (Show, Eq)

gsPlayer :: Lens GameState Player
gsPlayer = Lens _gsPlayer (\a s -> s { _gsPlayer = a })

gsGrid :: Lens GameState (Grid GridBead)
gsGrid = Lens _gsGrid (\a s -> s { _gsGrid = a })

isValidPlayerPosition :: GameState -> Bool
isValidPlayerPosition gameState =
    case gridGet (gameState^.gsGrid) (gameState^.gsPlayer^.pPosition) of
        Just something -> something == Empty
        _ -> False

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
