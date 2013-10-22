module GameLogic.View.Light ( multLights
                            , beadDiffuse
                            , lightIntensity
                            , getR
                            , getG
                            , getB
                            ) where

import Prelude ( Int
               , Double
               , otherwise
               , fromIntegral
               , snd
               , round
               , foldr
               , ($)
               , (>)
               , (/)
               , (*)
               , (+)
               )

import GameLogic.Color ( Color(..)
                       , BeadColor(..)
                       )

import GameLogic.Types ( Light(..)
                       )

getR, getG, getB :: (Light, Int) -> Int
getR (Light _ (r, _, _), _) = r
getG (Light _ (_, g, _), _) = g
getB (Light _ (_, _, b), _) = b

lightIntensity :: ((Light, Int) -> Int) -> (Light, Int) -> Double
lightIntensity f light@(Light radius _, _)
    | getDist light > radius = 0.0
    | otherwise = fromIntegral (f light) / fromIntegral (getDist light + 1)
  where
    getDist = snd

multLights :: (Double, Double, Double) -> Color -> [(Light, Int)] -> Color
multLights (dr, dg, db) (r, g, b) lights = (sumR, sumG, sumB)
  where
    sum :: Double -> ((Light, Int) -> Int) -> Int
    sum diffuse f = round $ diffuse * ((foldr (\light -> (+ intensity light))) 0.0 lights)
      where
        intensity :: (Light, Int) -> Double
        intensity light = lightIntensity f light

    sumR = r + sum dr getR
    sumG = g + sum dg getG
    sumB = b + sum db getB

beadDiffuse :: BeadColor -> (Double, Double, Double)
beadDiffuse (EmptyColor) = (1.0, 1.0, 1.0)
beadDiffuse (PlayerColor) = (1.0, 0.1, 0.1)
beadDiffuse (WallColor dist) =
    case dist of
        0 -> (0.1, 0.1, 0.1)
        1 -> (0.4, 0.4, 0.4)
        _ -> (0.5, 0.5, 0.5)
beadDiffuse (DoorColor dist) =
    case dist of
        0 -> (0.1, 0.1, 1.0)
        1 -> (0.4, 0.4, 1.0)
        _ -> (0.5, 0.5, 1.0)
