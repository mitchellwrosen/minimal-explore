module GameLogic.State.Internal ( leftButtonPressed
                                , rightButtonPressed
                                , upButtonPressed
                                , downButtonPressed
                                , forwardButtonPressed
                                , reverseButtonPressed
                                , loadNewRoom
                                , processLightMove
                                , gameStatePlayer
                                , gameStateGameMap
                                , gameStateGameMaps
                                , makeGameState
                                , GameState
                                ) where

import Prelude ( Show
               , Eq
               , String
               , Bool(..)
               , Maybe(..)
               , (==)
               , (++)
               , ($)
               , (.)
               , maybe
               , snd
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
import GameLogic.Player ( Player
                        , makePlayer
                        , playerFacing
                        , playerPosition
                        , playerApplyMove
                        , playerChangeDirection
                        )
import GameLogic.Grid ( Grid(..)
                      , gridGet
                      )
import GameLogic.Types ( GridBead(..)
                       , Door(..)
                       , Light(..)
                       , Position(..)
                       , Facing(..)
                       )
import GameLogic.GameMap ( getGameMapFromDoor
                         , getMatchingDoorPosition
                         , gameMapApplyMoveLight
                         , gameMapGrid
                         , gameMapLights
                         , GameMap
                         )
import qualified Levels.GameMaps

import Data.Util.Maybe ( fromMaybe )
import Control.Lens ( (^.)
                    , over
                    , Lens(..)
                    )

data GameState = GameState { _gameStatePlayer :: Player
                           , _gameStateGameMap :: GameMap
                           , _gameStateGameMaps :: [(String, GameMap)]
                           }
  deriving (Show, Eq)
gameStatePlayer = Lens { view = _gameStatePlayer
                       , set = \player gameState -> gameState { _gameStatePlayer = player }
                       }
gameStateGameMap = Lens { view = _gameStateGameMap
                        , set = \gameMap gameState -> gameState { _gameStateGameMap = gameMap }
                        }
gameStateGameMaps = Lens { view = _gameStateGameMaps
                         , set = \gameMaps gameState -> gameState { _gameStateGameMaps = gameMaps }
                         }
makeGameState = GameState

loadNewRoom :: GameState -> GridBead -> GameState
loadNewRoom gameState door = gameState'
  where
    oldFacing = gameState ^. gameStatePlayer ^. playerFacing
    position = getMatchingDoorPosition (gameState ^. gameStateGameMap) newMap door
    player = makePlayer position oldFacing
    gameState' = GameState player newMap (gameState^.gameStateGameMaps)
    newMap = getGameMapFromDoor (gameState^.gameStateGameMaps) door

processLightMove :: GameState -> GameState -> (Light, Position) -> Facing -> Move -> GameState
processLightMove defGameState playerMovedGameState light@(_, pos) facing move =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        _     -> defGameState
  where
    pos' = move facing pos
    gridBead = fromMaybe Wall $ gridGet (playerMovedGameState^.gameStateGameMap^.gameMapGrid) pos'

    gameState = over gameStateGameMap (\gameMap -> gameMapApplyMoveLight gameMap light facing move) playerMovedGameState

    lights = filter (\(_, lightPos) -> lightPos == pos') $ playerMovedGameState^.gameStateGameMap^.gameMapLights
    resolveLightBeadCollisions = case lights of
        [] -> gameState
        _  -> defGameState

processPlayerMove :: Move -> GameState -> GameState
processPlayerMove move gameState@(GameState player gameMap _) =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        door@(DoorBead _) -> loadNewRoom gameState door
        Wall -> gameState
  where
    gameState' = over gameStatePlayer (flip playerApplyMove move) gameState
    player' = gameState'^.gameStatePlayer
    gridBead = fromMaybe Wall $ gridGet (gameMap^.gameMapGrid) (player'^.playerPosition)

    light = filter ((== player'^.playerPosition) . snd) $ gameMap^.gameMapLights
    resolveLightBeadCollisions = case light of
        [light] -> processLightMove gameState gameState' light (player'^.playerFacing) move
        []      -> gameState'

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
