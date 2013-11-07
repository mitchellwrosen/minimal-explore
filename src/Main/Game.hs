module Main.Game () where

import Prelude

import Main.View
    ( drawMap
    )
import Main.Ref
    ( modifyRef
    , newRef
    , readRef
    , writeRef
    )
import Main.Perlenspiel
    ( setPSEvent
    , setPSMouseEvent
    , setPSKeyEvent
    , psGridSize
    , psGridColor
    )

import qualified Levels.Level2
import qualified Levels.GameMaps
import GameLogic.Types
    ( GridX
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
import GameLogic.Grid
    ( Grid(..)
    , gridDimensions
    )
import GameLogic.GameMap
    ( gameMapGrid
    )
import GameLogic.State
    ( leftButtonPressed
    , rightButtonPressed
    , upButtonPressed
    , downButtonPressed
    , forwardButtonPressed
    , backwardButtonPressed
    , reverseButtonPressed
    )

import Control.Lens
    ( (^.) )

-- Everything is in main for the beautiful closure that it offers over stateRef.
main :: Fay ()
main = do
    let player = makePlayer (1, 1, 2) Positive
        gameState = makeGameState player Levels.Level2.gameMap Levels.GameMaps.gameMaps
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
        psKeyDown keyValue' shift ctrl = return ()

        psOnKeyDown :: KeyValue -> Bool -> Bool -> Fay ()
        psOnKeyDown keyValue' shift ctrl = do
            let -- Convert the keyvalue to lowercase first.
                keyValue =
                    if keyValue' >= 65 + 32 && keyValue' <= 90 + 32 
                    then keyValue' - 32
                    else keyValue'
                move = case keyValue of
                       87 -> upButtonPressed
                       65 -> leftButtonPressed
                       83 -> downButtonPressed
                       68 -> rightButtonPressed
                       1006 -> forwardButtonPressed
                       1005 -> reverseButtonPressed
                       1007 -> reverseButtonPressed
                       1008 -> backwardButtonPressed
                       _ -> \_ -> id
                pullKeyDown = shift
            modifyRef stateRef (move pullKeyDown)
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
    setPSKeyEvent "onKeyDown" psOnKeyDown
    setPSKeyEvent "keyDown" psKeyDown
    setPSKeyEvent "keyUp" psKeyUp
