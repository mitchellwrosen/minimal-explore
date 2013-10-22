module Main.Perlenspiel ( setPSEvent
                        , setPSMouseEvent
                        , setPSMouseWheelEvent
                        , setPSKeyEvent
                        , psGridSize
                        , psGridColor
                        , psBeadColor
                        , psBorderColor
                        , psAll
                        ) where

import Prelude ( String
               , Int
               , Bool
               , Fay
               )

import qualified GameLogic.Color as Color
import GameLogic.Types ( GridX
                       , GridY
                       , BeadData
                       , KeyValue
                       , MWheelDelta
                       )

import FFI (ffi)

psAll :: Int
psAll = ffi "PS.ALL"

psBorderColor' :: Int -> Int -> [Int] -> Fay ()
psBorderColor' = ffi "PS.borderColor(%1, %2, %3)"

psBorderColor :: Int -> Int -> Color.Color -> Fay ()
psBorderColor x y color = psBorderColor' x y (Color.toList color)

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

psBeadColor' :: Int -> Int -> [Int] -> Fay ()
psBeadColor' = ffi "PS.color(%1, %2, %3)"

psBeadColor :: Int -> Int -> Color.Color -> Fay ()
psBeadColor x y color = psBeadColor' x y (Color.toList color)

psGridColor' :: [Int] -> Fay ()
psGridColor' = ffi "PS.gridColor(%1)"

psGridColor :: Color.Color -> Fay ()
psGridColor color = psGridColor' (Color.toList color)
