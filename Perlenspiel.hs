module Perlenspiel where

import Prelude
import Types
import FFI

setPSEvent :: String -> Fay () -> Fay ()
setPSEvent = ffi "PS[%1] = %2"

setPSMouseEvent :: String -> (GridX -> GridY -> BeadData -> Fay ()) -> Fay ()
setPSMouseEvent = ffi "PS[%1] = %2"

setPSMouseWheelEvent :: String -> (MWheelDelta -> Fay ()) -> Fay ()
setPSMouseWheelEvent = ffi "PS[%1] = %2"

setPSKeyEvent :: String -> (KeyValue -> Bool -> Bool -> Fay ()) -> Fay ()
setPSKeyEvent = ffi "PS[%1] = %2"

psGridSize :: Int -> Int -> Fay()
psGridSize = ffi "PS.GridSize(%1, %2)"
