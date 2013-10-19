module Main.Perlenspiel ( setPSEvent
                        , setPSMouseEvent
                        , setPSMouseWheelEvent
                        , setPSKeyEvent
                        , psGridSize
                        , psBeadColor
                        ) where

import Prelude ( String
               , Int
               , Bool
               , Fay
               )

import GameLogic.Types ( GridX
                       , GridY
                       , BeadData
                       , KeyValue
                       , MWheelDelta
                       )

import FFI (ffi)

setPSEvent :: String -> Fay () -> Fay ()
setPSEvent = ffi "PS[%1] = %2"

setPSMouseEvent :: String -> (GridX -> GridY -> BeadData -> Fay ()) -> Fay ()
setPSMouseEvent = ffi "PS[%1] = %2"

setPSMouseWheelEvent :: String -> (MWheelDelta -> Fay ()) -> Fay ()
setPSMouseWheelEvent = ffi "PS[%1] = %2"

setPSKeyEvent :: String -> (KeyValue -> Bool -> Bool -> Fay ()) -> Fay ()
setPSKeyEvent = ffi "PS[%1] = %2"

psGridSize :: Int -> Int -> Fay()
psGridSize = ffi "PS.gridSize(%1, %2)"

psBeadColor :: Int -> Int -> [Int] -> Fay ()
psBeadColor = ffi "PS.color(%1, %2, %3)"
