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
               , ($)
               , (.)
               , maybe
               , flip
               , lookup
               , filter
               , id
               , error
               )

import GameLogic.Move ( moveLeft
                      , moveRight
                      , moveUp
                      , moveDown
                      , moveForward
                      , Move(..)
                      )
import GameLogic.Player ( Player(..)
                        , playerGetPosition
                        , playerApplyMove
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
                         , gameMapApplyMoveLight
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

processPlayerMove :: Move -> GameState -> GameState
processPlayerMove move gameState@(GameState player gameMap) =
    case gridBead of
        Empty -> checkLightBeadCollisions
        door@(DoorBead _) -> loadNewRoom gameState door
        Wall -> gameState
  where
    fromMaybe = flip maybe id
    player' = playerApplyMove player move

    gameState' = GameState player' gameMap
    (x, y, z) = playerGetPosition player'
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid $ gameMap) x y z

    checkLightBeadCollisions = case light of
        [light] ->
            gameState' { gameStateGameMap = gameMapApplyMoveLight gameMap light facing move }
        []      -> gameState'
      where
        light = filter (\(_, lightPos) -> lightPos == (x, y, z)) $ gameMapLights gameMap
        facing = _facing player'

leftButtonPressed :: GameState -> GameState
leftButtonPressed = processPlayerMove moveLeft

rightButtonPressed :: GameState -> GameState
rightButtonPressed = processPlayerMove moveRight

upButtonPressed :: GameState -> GameState
upButtonPressed = processPlayerMove moveUp

downButtonPressed :: GameState -> GameState
downButtonPressed = processPlayerMove moveDown

forwardButtonPressed :: GameState -> GameState
forwardButtonPressed = processPlayerMove moveForward

reverseButtonPressed :: GameState -> GameState
reverseButtonPressed gameState =
    gameState { _player = playerChangeDirection $ _player gameState }
