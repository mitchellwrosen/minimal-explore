module Main.View (drawMap) where

import Prelude ( Int
               , Double
               , Fay
               , replicate
               , map
               , fst
               , snd
               , flip
               , div
               , foldr
               , sequence_
               , zipWith
               , sum
               , round
               , fromIntegral
               , (*)
               , (+)
               , (/)
               , (.)
               , ($)
               , (++)
               , (!!)
               )

import Main.Perlenspiel ( psBeadColor
                        )

import GameLogic.Types ( Color(..)
                       , GridBead(..)
                       )
import GameLogic.State ( GameState(..)
                       )
import GameLogic.View ( getView
                      )

maxLight :: Int
maxLight = 64

fadeValue :: Int -> Int
fadeValue 0 = 0
fadeValue 1 = round $ fromIntegral maxLight * 64 / 255
fadeValue 2 = round $ fromIntegral maxLight * 128 / 255
fadeValue 3 = round $ fromIntegral maxLight * 192 / 255
fadeValue _ = maxLight

multLights :: (Double, Double, Double) -> [Int] -> [(GridBead, Int)] -> [Int]
multLights (dr, dg, db) [r, g, b] lights = [sumR, sumG, sumB]
  where
    getR (Light _ (r, _, _), _) = r
    getG (Light _ (_, g, _), _) = g
    getB (Light _ (_, _, b), _) = b

    getDist = snd

    sum :: Double -> ((GridBead, Int) -> Int) -> Int
    sum diffuse f = round $ diffuse * ((foldr (\light -> (+ intensity light))) 0.0 lights)
      where
        intensity :: (GridBead, Int) -> Double
        intensity light = fromIntegral (f light) / fromIntegral (getDist light + 1)

    sumR = r + sum dr getR
    sumG = g + sum dg getG
    sumB = b + sum db getB

beadDiffuse :: Color -> (Double, Double, Double)
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

beadColor :: (Color, [(GridBead, Int)]) -> [Int]
beadColor (color@(EmptyColor), lights) =
    multLights (beadDiffuse color) [maxLight, maxLight, maxLight] lights
beadColor (color@(PlayerColor), lights) =
    multLights (beadDiffuse color) [maxLight, 0, 0] lights
beadColor (color@(WallColor dist), lights) =
    multLights (beadDiffuse color) (replicate 3 $ fadeValue dist) lights
beadColor (color@(DoorColor dist), lights) =
    multLights (beadDiffuse color) ((replicate 2 $ fadeValue dist) ++ [maxLight]) lights

drawMap :: GameState -> Fay ()
drawMap gameState = do
    let view = getView gameState
        colorView = map (map (beadColor)) view
    mapMInd_ (mapMInd_ . flip psBeadColor) colorView
  where
    mapMInd_ :: (Int -> a -> Fay b) -> [a] -> Fay ()
    mapMInd_ f = zipWithM_ f [0..]

    zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
    zipWithM_ f xs ys = sequence_ (zipWith f xs ys)
