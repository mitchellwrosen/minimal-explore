module Main.Game () where

import Prelude ( Fay
               , Bool
               , id
               , return
               , replicate
               , (>>=)
               )

import Main.View ( drawMap
                 )
import Main.Ref ( modifyRef
                , newRef
                , readRef
                )
import Main.Perlenspiel ( setPSEvent
                        , setPSMouseEvent
                        , setPSKeyEvent
                        , psGridSize
                        , psGridColor
                        )

import qualified Levels.Level1
import qualified Levels.GameMaps
import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , KeyValue
                       , MWheelDelta
                       , BeadData
                       , GridBead(..)
                       , Facing(..)
                       )
import GameLogic.Types.Player
    ( makePlayer
    )
import GameLogic.Types.GameState
    ( makeGameState
    , gameStateGameMap
    )
import GameLogic.Grid ( Grid(..)
                      , gridDimensions
                      )
import GameLogic.GameMap ( gameMapGrid
                         )
import GameLogic.State ( leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       )

import Control.Lens ( (^.) )

-- Everything is in main for the beautiful closure that it offers over stateRef.
main :: Fay ()
main = do
    let player = makePlayer (0, 2, 2) Positive
        gameState = makeGameState player Levels.Level1.gameMap Levels.GameMaps.gameMaps
    stateRef <- newRef gameState

    let psInit :: Fay ()
        psInit = do
            gameState <- readRef stateRef
            let (_, viewHeight, viewWidth) = gridDimensions (gameState^.gameStateGameMap^.gameMapGrid)
            psGridSize viewWidth viewHeight
            psGridColor (15, 15, 15)
            drawMap gameState

        psTouch :: GridX -> GridY -> BeadData -> Fay ()
        psTouch x y beadData = return ()

        psRelease :: GridX -> GridY -> BeadData -> Fay ()
        psRelease x y beadData = return ()

        psEnter :: GridX -> GridY -> BeadData -> Fay ()
        psEnter x y beadData = return ()

        psExit :: GridX -> GridY -> BeadData -> Fay ()
        psExit x y beadData = return ()

        psExitGrid :: Fay ()
        psExitGrid = return ()

        psInput :: Fay ()
        psInput = return ()

        psKeyDown :: KeyValue -> Bool -> Bool -> Fay ()
        psKeyDown keyValue shift ctrl = do
            let move = case keyValue of
                    87 -> upButtonPressed
                    65 -> leftButtonPressed
                    83 -> downButtonPressed
                    68 -> rightButtonPressed
                    32 -> forwardButtonPressed
                    74 -> reverseButtonPressed
                    _ -> id
            modifyRef stateRef move
            readRef stateRef >>= drawMap

        psKeyUp :: KeyValue -> Bool -> Bool -> Fay ()
        psKeyUp keyValue shift ctrl = return ()

    setPSEvent "init" psInit
    setPSEvent "exitGrid" psExitGrid
    setPSEvent "input" psInput
    setPSMouseEvent "touch" psTouch
    setPSMouseEvent "release" psRelease
    setPSMouseEvent "enter" psEnter
    setPSMouseEvent "exit" psExit
    setPSKeyEvent "keyDown" psKeyDown
    setPSKeyEvent "keyUp" psKeyUp
