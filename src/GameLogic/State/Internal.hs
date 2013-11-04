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

import GameLogic.Move ( Move(..)
                      , applyMove
                      , oppositeMove
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
                       , Gate(..)
                       , Position
                       )
import GameLogic.Types.GameState
    ( GameState
    , makeGameState
    , gameStatePlayer
    , gameStateGameMap
    , gameStateGameMaps
    )

import GameLogic.Types.Player
    ( makePlayer
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
                    , (.~)
                    , over
                    )

loadNewRoom :: GameState -> GridBead -> GameState
loadNewRoom gameState door = gameState'
  where
    oldFacing = gameState ^. gameStatePlayer ^. playerFacing
    position = getMatchingDoorPosition (gameState ^. gameStateGameMap) newMap door
    player = makePlayer position oldFacing
    gameState' = makeGameState player newMap (gameState^.gameStateGameMaps)
    newMap = getGameMapFromDoor (gameState^.gameStateGameMaps) door

isGateOpen :: GameState -> Position -> Gate -> Bool
isGateOpen gameState position gate =
    gateColor gate == gateLightingColor
  where
    gateLightingColor = getColorViewAt gameState position

processLightMove :: GameState -> GameState -> (Light, Position) -> Move -> GameState
processLightMove defGameState playerMovedGameState light@(_, pos) move =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        GateBead gate ->
            if isGateOpen defGameState pos' gate
            then resolveLightBeadCollisions
            else defGameState
        _     -> defGameState
  where
    facing = defGameState^.gameStatePlayer^.playerFacing
    pos' = applyMove move facing pos
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
        doorBead@(DoorBead door) -> if isLightOpen door
                                    then loadNewRoom gameState doorBead
                                    else gameState
        GateBead gate -> if isGateOpen gameState (player'^.playerPosition) gate
                         then gameState'
                         else if length lights == 1
                              then resolveLightBeadCollisions
                              else gameState
        Wall -> gameState
        _ -> error "Should not contain light beads"
  where
    isLightOpen :: Door -> Bool
    isLightOpen door = doorColor door == doorLightingColor
      where
        doorLightingColor = getColorViewAt gameState (player'^.playerPosition)

    gameMap = gameState^.gameStateGameMap
    gameState' = over gameStatePlayer (flip playerApplyMove move) gameState
    player' = gameState'^.gameStatePlayer
    gridBead = fromMaybe Wall $ gridGet (gameMap^.gameMapGrid) (player'^.playerPosition)

    lights = filter ((== player'^.playerPosition) . snd) $ gameMap^.gameMapLights
    resolveLightBeadCollisions = case lights of
        [lite] -> processLightMove gameState gameState' lite move
        []      -> gameState'
        _ -> error "2 lights should not have the same position"

leftButtonPressed :: Bool -> GameState -> GameState
leftButtonPressed = processPlayerMove MoveLeft

rightButtonPressed :: Bool -> GameState -> GameState
rightButtonPressed = processPlayerMove MoveRight

upButtonPressed :: Bool -> GameState -> GameState
upButtonPressed = processPlayerMove MoveUp

downButtonPressed :: Bool -> GameState -> GameState
downButtonPressed = processPlayerMove MoveDown

forwardButtonPressed :: Bool -> GameState -> GameState
forwardButtonPressed = processPlayerMove MoveForward

reverseButtonPressed :: Bool -> GameState -> GameState
reverseButtonPressed _ = over gameStatePlayer playerChangeDirection
