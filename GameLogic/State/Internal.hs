module GameLogic.State.Internal ( leftButtonPressed
                                , rightButtonPressed
                                , upButtonPressed
                                , downButtonPressed
                                , forwardButtonPressed
                                , reverseButtonPressed
                                , GameState(..)
                                ) where

import Prelude ( Show
               , Eq
               , Bool(..)
               , Maybe(..)
               , (==)
               , (++)
               , maybe
               , lookup
               , id
               , error
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
                       , Door(..)
                       )
import GameLogic.GameMap ( getGameMapFromDoor
                         , getMatchingDoorPosition
                         , GameMap(..)
                         )
import qualified Levels.GameMaps

data GameState = GameState { _player :: Player
                           , gameStateGameMap :: GameMap
                           }
  deriving (Show, Eq)

loadNewRoom :: GameState -> GridBead -> GameState
loadNewRoom gameState door = gameState'
  where
    oldFacing = _facing (_player gameState)
    position = getMatchingDoorPosition (gameStateGameMap gameState) newMap door
    player = Player position oldFacing
    gameState' = GameState player newMap
    newMap = getGameMapFromDoor Levels.GameMaps.gameMaps door

processPlayerMove :: (Player -> Player) -> GameState -> GameState
processPlayerMove playerMove gameState@(GameState player grid) =
    case maybeGridBead of
        Just Empty -> gameState'
        Just door@(DoorBead _) -> loadNewRoom gameState door
        _ -> gameState
  where
    gameState' = GameState (playerMove player) grid
    (x, y, z) = playerGetPosition (playerMove (_player gameState))
    maybeGridBead = gridGet (gameMapGrid (gameStateGameMap gameState)) x y z

leftButtonPressed :: GameState -> GameState
leftButtonPressed = processPlayerMove playerMoveLeft

rightButtonPressed :: GameState -> GameState
rightButtonPressed = processPlayerMove playerMoveRight

upButtonPressed :: GameState -> GameState
upButtonPressed = processPlayerMove playerMoveUp

downButtonPressed :: GameState -> GameState
downButtonPressed = processPlayerMove playerMoveDown

forwardButtonPressed :: GameState -> GameState
forwardButtonPressed = processPlayerMove playerMoveForward

reverseButtonPressed :: GameState -> GameState
reverseButtonPressed = processPlayerMove playerChangeDirection
