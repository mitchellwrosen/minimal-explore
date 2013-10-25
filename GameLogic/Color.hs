module GameLogic.Color ( toList
                       , fromList
                       , ambientColor
                       ) where

import Prelude ( Int
               , Show
               , Eq
               , round
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
ambientColor maxLight (PlayerColor) = (maxLight, 0, 0)
ambientColor maxLight (DoorColor dist) = (maxLight, maxLight, maxLight)
ambientColor maxLight (WallColor dist) = (wallFadeValue dist, wallFadeValue dist, wallFadeValue dist)
  where
    wallFadeValue :: Int -> Int
    wallFadeValue 0 = 0
    wallFadeValue 1 = round $ fromIntegral maxLight * 64 / 255
    wallFadeValue 2 = round $ fromIntegral maxLight * 128 / 255
    wallFadeValue 3 = round $ fromIntegral maxLight * 192 / 255
    wallFadeValue _ = maxLight
