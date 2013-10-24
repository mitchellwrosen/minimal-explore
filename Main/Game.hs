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
                        , setPSMouseWheelEvent
                        , setPSKeyEvent
                        , psGridSize
                        , psGridColor
                        , psBeadColor
                        , psBorderColor
                        , psAll
                        )

import qualified Levels.Level1
import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , KeyValue
                       , MWheelDelta
                       , BeadData
                       , GridBead(..)
                       )
import GameLogic.Grid ( Grid(..)
                      , gridDimensions
                      )
import GameLogic.Move ( Facing(..)
                      )
import GameLogic.Player ( Player(..)
                        )
import GameLogic.GameMap ( gameMapGrid
                         )
import GameLogic.State ( GameState(..)
                       , leftButtonPressed
                       , rightButtonPressed
                       , upButtonPressed
                       , downButtonPressed
                       , forwardButtonPressed
                       , reverseButtonPressed
                       )

-- Everything is in main for the beautiful closure that it offers over stateRef.
main :: Fay ()
main = do
    let player = Player (0, 2, 2) Positive
        gameState = GameState player Levels.Level1.gameMap
    stateRef <- newRef gameState

    let psInit :: Fay ()
        psInit = do
            gameState <- readRef stateRef
            let (_, viewHeight, viewWidth) = gridDimensions (gameMapGrid (gameStateGameMap gameState))
            psGridSize viewWidth viewHeight
            psGridColor (15, 15, 15)
            psBorderColor psAll psAll (15, 15, 15)
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
