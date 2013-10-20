module Main.Game () where

import Prelude ( Fay
               , Bool
               , Int
               , id
               , map
               , flip
               , zipWith
               , sequence_
               , return
               , print
               , undefined
               , (.)
               , (>>=)
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
                        , psBeadColor
                        )

import qualified Levels.Level1
import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , KeyValue
                       , MWheelDelta
                       , BeadData
                       , GridBead(..)
                       , Color(..)
                       )
import GameLogic.Grid ( Grid(..)
                      , gridDimensions
                      )
import GameLogic.Player ( Player(..)
                        , Facing(..)
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
import GameLogic.View ( getView
                      )

beadColor :: Color -> [Int]
beadColor (EmptyColor) = [255, 255, 255]
beadColor (WallColor dist) =
    case dist of
        0 -> [0, 0, 0]
        1 -> [64, 64, 64]
        2 -> [128, 128, 128]
        3 -> [192, 192, 192]
        _ -> beadColor EmptyColor
beadColor (PlayerColor) = [255, 0, 0]
beadColor (DoorColor) = [0, 0, 255]

drawMap :: GameState -> Fay ()
drawMap gameState = do
    let view = getView gameState
        colorView = map (map beadColor) view
    mapMInd_ (mapMInd_ . flip psBeadColor) colorView
  where
    mapMInd_ :: (Int -> a -> Fay b) -> [a] -> Fay ()
    mapMInd_ f = zipWithM_ f [0..]

    zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
    zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-- Everything is in main for the beautiful closure that it offers over stateRef.
main :: Fay ()
main = do
    let player = Player (0, 1, 1) Positive
        gameState = GameState player Levels.Level1.gameMap
    stateRef <- newRef gameState

    let psInit :: Fay ()
        psInit = do
            gameState <- readRef stateRef
            let (_, gridHeight, gridWidth) = gridDimensions (gameMapGrid (gameStateGameMap gameState))
            psGridSize gridWidth gridHeight
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
