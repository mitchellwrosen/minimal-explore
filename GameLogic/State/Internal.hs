module GameLogic.State.Internal ( leftButtonPressed
                                , rightButtonPressed
                                , upButtonPressed
                                , downButtonPressed
                                , forwardButtonPressed
                                , reverseButtonPressed
                                , gameStatePlayer
                                , gameStateGameMap
                                , makeGameState
                                , GameState
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
                      , Facing(..)
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
                       )
import GameLogic.GameMap ( getGameMapFromDoor
                         , getMatchingDoorPosition
                         , gameMapApplyMoveLight
                         , GameMap(..)
                         )
import qualified Levels.GameMaps

import Control.Lens ( (^.)
                    , over
                    , Lens(..)
                    )

data GameState = GameState { _gameStatePlayer :: Player
                           , _gameStateGameMap :: GameMap
                           }
  deriving (Show, Eq)
gameStatePlayer = Lens { view = \(GameState player _) -> player
                       , set  = \player (GameState _ gameMap) -> GameState player gameMap
                       }
gameStateGameMap = Lens { view = \(GameState _ gameMap) -> gameMap
                        , set  = \gameMap (GameState player _) -> GameState player gameMap
                        }
makeGameState = GameState

loadNewRoom :: GameState -> GridBead -> GameState
loadNewRoom gameState door = gameState'
  where
    -- TODO(R): lenses
    oldFacing = gameState ^. gameStatePlayer ^. playerFacing
    position = getMatchingDoorPosition (gameState ^. gameStateGameMap) newMap door
    player = makePlayer position oldFacing
    gameState' = GameState player newMap
    newMap = getGameMapFromDoor Levels.GameMaps.gameMaps door

-- TODO(R): helper module
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

processLightMove :: GameState -> GameState -> (Light, Position) -> Facing -> Move -> GameState
processLightMove defGameState playerMovedGameState light@(_, pos) facing move =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        _     -> defGameState
  where
    -- TODO(R): nearly identical code below. refactor
    (x, y, z) = move facing pos

    gameMap = playerMovedGameState ^. gameStateGameMap
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid gameMap) x y z
    gameState = playerMovedGameState { _gameStateGameMap = gameMapApplyMoveLight gameMap light facing move }
    resolveLightBeadCollisions = case lights of
        [] -> gameState
        _ -> defGameState
      where
        lights = filter (\(_, lightPos) -> lightPos == (x, y, z)) $ gameMapLights gameMap

processPlayerMove :: Move -> GameState -> GameState
processPlayerMove move gameState@(GameState player gameMap) =
    case gridBead of
        Empty -> resolveLightBeadCollisions
        door@(DoorBead _) -> loadNewRoom gameState door
        Wall -> gameState
  where
    player' = playerApplyMove player move
    gameState' = GameState player' gameMap
    (x, y, z) = player' ^. playerPosition
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid $ gameMap) x y z

    resolveLightBeadCollisions = case light of
        [light] -> processLightMove gameState gameState' light facing move
        []      -> gameState'
      where
        facing = player' ^. playerFacing
        light = filter ((== (x, y, z)) . snd) $ gameMapLights gameMap

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
