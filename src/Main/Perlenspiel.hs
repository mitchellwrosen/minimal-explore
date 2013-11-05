module Main.Perlenspiel ( setPSEvent
                        , setPSMouseEvent
                        , setPSMouseWheelEvent
                        , setPSKeyEvent
                        , psGridSize
                        , psGridColor
                        , psBeadColor
                        , psBorderColor
                        , psBorderWidth
                        , psGlyph
                        , psGlyphFade
                        , psGlyphAlpha
                        , psGlyphColor
                        , psAll
                        , psRadius
                        , psStatusText
                        , psStatusColor
                        ) where

import Prelude

import qualified GameLogic.Color as Color
import GameLogic.Types ( GridX
                       , GridY
                       , BeadData
                       , KeyValue
                       , MWheelDelta
                       , Color(..)
                       )

import FFI (ffi)

psAll :: Int
psAll = ffi "PS.ALL"

psBorderColor' :: Int -> Int -> [Int] -> Fay ()
psBorderColor' = ffi "PS.borderColor(%1, %2, %3)"

psBorderColor :: Int -> Int -> Color -> Fay ()
psBorderColor x y color = psBorderColor' x y (Color.toList color)

psBorderWidth :: Int -> Int -> Int -> Fay ()
psBorderWidth = ffi "PS.border(%1, %2, %3)"

psRadius :: Int -> Int -> Int -> Fay ()
psRadius = ffi "PS.radius(%1, %2, %3)"

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

psBeadColor :: Int -> Int -> Color -> Fay ()
psBeadColor x y color = psBeadColor' x y (Color.toList color)

psGridColor' :: [Int] -> Fay ()
psGridColor' = ffi "PS.gridColor(%1)"

psGridColor :: Color -> Fay ()
psGridColor color = psGridColor' (Color.toList color)

psGlyph :: Int -> Int -> String -> Fay ()
psGlyph = ffi "PS.glyph(%1, %2, %3)"

psGlyphFade :: Int -> Int -> Int -> Fay ()
psGlyphFade = ffi "PS.glyphFade(%1, %2, %3)"

psGlyphAlpha :: Int -> Int -> Int -> Fay ()
psGlyphAlpha = ffi "PS.glyphAlpha(%1, %2, %3)"

psGlyphColor' :: Int -> Int -> [Int] -> Fay ()
psGlyphColor' = ffi "PS.glyphColor(%1, %2, %3)"

psGlyphColor :: Int -> Int -> Color -> Fay ()
psGlyphColor x y color = psGlyphColor' x y (Color.toList color)

psStatusText :: String -> Fay ()
psStatusText = ffi "PS.statusText(%1)"

psStatusColor' :: [Int] -> Fay ()
psStatusColor' = ffi "PS.statusColor(%1)"

psStatusColor :: Color -> Fay ()
psStatusColor color = psStatusColor' $ Color.toList color
