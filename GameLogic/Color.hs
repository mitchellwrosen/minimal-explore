module GameLogic.Color ( toList
                       , fromList
                       , ambientColor
                       ) where

import Prelude ( Int
               , Show
               , Eq
               , round
               , min
               , max
               , fromIntegral
               , (/)
               , (*)
               , ($)
               )

import GameLogic.Types ( Color
                       , Byte
                       , BeadColor(..)
                       )

toList :: Color -> [Byte]
toList (r, g, b) = [r, g, b]

fromList :: [Byte] -> Color
fromList [r, g, b] = (r, g, b)

ambientColor :: Byte -> BeadColor -> Color
ambientColor maxLight (EmptyColor) = (maxLight, maxLight, maxLight)
ambientColor maxLight (DoorColor dist) = (maxLight, maxLight, maxLight)
ambientColor maxLight (PlayerColor) = (maxLight, 0, 0)
ambientColor maxLight (WallColor dist) = (wallFadeValue dist, wallFadeValue dist, wallFadeValue dist)
  where
    fadeRatio num = round $ fromIntegral maxLight * num / 255
    clamp val = min (max val 0) 4

    wallFadeValue :: Int -> Int
    wallFadeValue num = fadeRatio (fromIntegral (clamp num) * 256 / 4)
