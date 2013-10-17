module Main.Game () where

import Prelude ( Fay
               , Bool
               , return
               )
import Main.Perlenspiel ( setPSEvent
                        , setPSMouseEvent
                        , setPSMouseWheelEvent
                        , setPSKeyEvent
                        , psGridSize
                        )

import GameLogic.Types ( GridX
                       , GridY
                       , KeyValue
                       , MWheelDelta
                       , BeadData
                       )

main :: Fay ()
main = do
    setPSEvent "Init" psInit
    setPSMouseEvent "Click" psClick
    setPSMouseEvent "Release" psRelease
    setPSMouseEvent "Enter" psEnter
    setPSMouseEvent "Leave" psLeave
    setPSMouseWheelEvent "Wheel" psWheel
    setPSKeyEvent "KeyDown" psKeyDown
    setPSKeyEvent "KeyUp" psKeyUp

psInit :: Fay ()
psInit = do
    psGridSize 10 10

psClick :: GridX -> GridY -> BeadData -> Fay ()
psClick x y beadData = return ()

psRelease :: GridX -> GridY -> BeadData -> Fay ()
psRelease x y beadData = return ()

psEnter :: GridX -> GridY -> BeadData -> Fay ()
psEnter x y beadData = return ()

psLeave :: GridX -> GridY -> BeadData -> Fay ()
psLeave x y beadData = return ()

psWheel :: MWheelDelta -> Fay ()
psWheel delta = return ()

psKeyDown :: KeyValue -> Bool -> Bool -> Fay ()
psKeyDown keyValue shift ctrl = return ()

psKeyUp :: KeyValue -> Bool -> Bool -> Fay ()
psKeyUp keyValue shift ctrl = return ()
