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
import GameLogic.View ( getColorView
                      )

drawMap :: GameState -> Fay ()
drawMap gameState =
    mapMInd_ (mapMInd_ . flip psBeadColor) (getColorView gameState)
  where
    mapMInd_ :: (Int -> a -> Fay b) -> [a] -> Fay ()
    mapMInd_ f = zipWithM_ f [0..]

    zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
    zipWithM_ f xs ys = sequence_ (zipWith f xs ys)
