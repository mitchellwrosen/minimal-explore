module GameLogic.Types.GameState ( gameStatePlayer
                                 , gameStateGameMap
                                 , gameStateGameMaps
                                 , makeGameState
                                 , GameState
                                 ) where

import Prelude
import GameLogic.Types.GameMap ( GameMap )
import GameLogic.Types.Player ( Player )

import Control.Lens ( Lens(..) )

data GameState = GameState { _gameStatePlayer :: Player
                           , _gameStateGameMap :: GameMap
                           , _gameStateGameMaps :: [(String, GameMap)]
                           }
  deriving (Show, Eq)
gameStatePlayer :: Lens GameState Player
gameStatePlayer = Lens { view = _gameStatePlayer
                       , set = \player gameState -> gameState { _gameStatePlayer = player }
                       }
gameStateGameMap :: Lens GameState GameMap
gameStateGameMap = Lens { view = _gameStateGameMap
                        , set = \gameMap gameState -> gameState { _gameStateGameMap = gameMap }
                        }
gameStateGameMaps :: Lens GameState [(String, GameMap)]
gameStateGameMaps = Lens { view = _gameStateGameMaps
                         , set = \gameMaps gameState -> gameState { _gameStateGameMaps = gameMaps }
                         }
makeGameState :: Player -> GameMap -> [(String, GameMap)] -> GameState
makeGameState = GameState
