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
                      , Facing(..)
                      , Position(..)
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
                       , Light(..)
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

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

processLightMove :: GameState -> GameState -> (Light, Position) -> Facing -> Move -> GameState
processLightMove defGameState playerMovedGameState light@(_, pos) facing move =
    case gridBead of
        Empty -> checkLightBeadCollisions
        _ -> defGameState
  where
    (x, y, z) = move facing pos

    gameMap = gameStateGameMap playerMovedGameState
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid gameMap) x y z
    gameState = playerMovedGameState { gameStateGameMap = gameMapApplyMoveLight gameMap light facing move }
    checkLightBeadCollisions = case lights of
        [] -> gameState
        _ -> defGameState
      where
        lights = filter (\(_, lightPos) -> lightPos == (x, y, z)) $ gameMapLights gameMap

processPlayerMove :: Move -> GameState -> GameState
processPlayerMove move gameState@(GameState player gameMap) =
    case gridBead of
        Empty -> checkLightBeadCollisions
        door@(DoorBead _) -> loadNewRoom gameState door
        Wall -> gameState
  where
    player' = playerApplyMove player move
    gameState' = GameState player' gameMap
    (x, y, z) = playerGetPosition player'
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid $ gameMap) x y z

    checkLightBeadCollisions = case light of
        [light] -> processLightMove gameState gameState' light facing move
        [] -> gameState'
      where
        facing = _facing player'
        light = filter (\(_, lightPos) -> lightPos == (x, y, z)) $ gameMapLights gameMap

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
