module GameLogic.Color
    ( toList
    , toTriple
    , fromList
    , ambientColor
    ) where

import Prelude

import GameLogic.Types
    ( Color
    , colorR
    , colorG
    , colorB
    , makeColor
    , Byte
    , BeadColor(..)
    )
import Control.Lens
    ( (^.) )

toTriple :: Color -> (Byte, Byte, Byte)
toTriple color = (color^.colorR, color^.colorG, color^.colorB)

toList :: Color -> [Byte]
toList color = [r, g, b]
  where (r, g, b) = toTriple color

fromList :: [Byte] -> Color
fromList [r, g, b] = makeColor (r, g, b)
fromList _ = error "Badly formed color list"

ambientColor :: Byte -> BeadColor -> Color
ambientColor maxLight (EmptyColor) = makeColor (maxLight, maxLight, maxLight)
ambientColor maxLight (DoorColor _) = makeColor (maxLight, maxLight, maxLight)
ambientColor maxLight (GateColor _) = makeColor (maxLight, maxLight, maxLight)
ambientColor maxLight (PlayerColor) = makeColor (maxLight, 0, 0)
ambientColor maxLight (WallColor dist) = makeColor (wallFadeValue dist, wallFadeValue dist, wallFadeValue dist)
  where
    fadeRatio :: Double -> Int
    fadeRatio num = round $ fromIntegral maxLight * num / 255
    clamp val = min (max val 0) 4

    wallFadeValue :: Int -> Int
    wallFadeValue num = fadeRatio (fromIntegral (clamp num) * 256 / 4)
ambientColor _ _ = error "light beads do not have an ambient color"
