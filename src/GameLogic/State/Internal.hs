module GameLogic.State.Internal ( leftButtonPressed
                                , rightButtonPressed
                                , upButtonPressed
                                , downButtonPressed
                                , forwardButtonPressed
                                , reverseButtonPressed
                                , loadNewRoom
                                , processLightMove
                                ) where

import Prelude

import GameLogic.Move ( moveLeft
                      , moveRight
                      , moveUp
                      , moveDown
                      , moveForward
                      , Move
                      )
import GameLogic.Player ( playerApplyMove
                        , playerChangeDirection
                        )
import GameLogic.View ( getColorViewAt
                      )
import GameLogic.Grid ( gridGet
                      )
import GameLogic.Types ( GridBead(..)
                       , Light(..)
                       , Door(..)
                       , GameState(..)
                       , gameStatePlayer
                       , gameStateGameMap
                       , gameStateGameMaps
                       , Position
                       , makePlayer
                       , playerFacing
                       , playerPosition
                       )
import GameLogic.GameMap ( getGameMapFromDoor
                         , getMatchingDoorPosition
                         , gameMapApplyMoveLight
                         , gameMapGrid
                         , gameMapLights
                         )

import Data.Util.Maybe ( fromMaybe )
import Control.Lens ( (^.)
                    , over
                    )

loadNewRoom :: GameState -> GridBead -> GameState
loadNewRoom gameState door = gameState'
  where
    oldFacing = gameState ^. gameStatePlayer ^. playerFacing
    position = getMatchingDoorPosition (gameState ^. gameStateGameMap) newMap door
    player = makePlayer position oldFacing
    gameState' = GameState player newMap (gameState^.gameStateGameMaps)
    newMap = getGameMapFromDoor (gameState^.gameStateGameMaps) door

processLightMove :: GameState -> GameState -> (Light, Position) -> Move -> GameState
processLightMove defGameState playerMovedGameState light@(_, pos) move =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        _     -> defGameState
  where
    facing = defGameState^.gameStatePlayer^.playerFacing
    pos' = move facing pos
    gridBead = fromMaybe Wall $ gridGet (playerMovedGameState^.gameStateGameMap^.gameMapGrid) pos'

    gameState = over gameStateGameMap (\gameMap -> gameMapApplyMoveLight gameMap light facing move) playerMovedGameState

    lights = filter (\(_, lightPos) -> lightPos == pos') $ playerMovedGameState^.gameStateGameMap^.gameMapLights
    resolveLightBeadCollisions = case lights of
        [] -> gameState
        _  -> defGameState

processPlayerMove :: Move -> GameState -> GameState
processPlayerMove move gameState =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        doorBead@(DoorBead door) -> if checkDoorLighting door
                                    then loadNewRoom gameState doorBead
                                    else gameState
        Wall -> gameState
        _ -> error "Should not contain light beads"
  where
    checkDoorLighting :: Door -> Bool
    checkDoorLighting door = doorColor door == doorLightingColor
      where
        doorLightingColor = getColorViewAt gameState (player'^.playerPosition)

    gameMap = gameState^.gameStateGameMap
    gameState' = over gameStatePlayer (flip playerApplyMove move) gameState
    player' = gameState'^.gameStatePlayer
    gridBead = fromMaybe Wall $ gridGet (gameMap^.gameMapGrid) (player'^.playerPosition)

    light = filter ((== player'^.playerPosition) . snd) $ gameMap^.gameMapLights
    resolveLightBeadCollisions = case light of
        [lite] -> processLightMove gameState gameState' lite move
        []      -> gameState'
        _ -> error "2 lights should not have the same position"

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
reverseButtonPressed = over gameStatePlayer playerChangeDirection
