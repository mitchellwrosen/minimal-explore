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
               , maybe
               , flip
               , lookup
               , filter
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
processPlayerMove playerMove gameState@(GameState player gameMap) =
    case gridBead of
        Empty -> checkLightBeadCollisions
        door@(DoorBead _) -> loadNewRoom gameState door
        Wall -> gameState
  where
    fromMaybe = flip maybe id

    gameState' = GameState (playerMove player) gameMap
    (x, y, z) = playerGetPosition $ playerMove player
    gridBead = fromMaybe Wall $ gridGet (gameMapGrid $ gameMap) x y z

    checkLightBeadCollisions = case light of
        [light] -> gameState
        []      -> gameState'
      where
        light = filter (\(_, lightPos) -> lightPos == (x, y, z)) $ gameMapLights gameMap

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
