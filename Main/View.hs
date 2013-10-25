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
               , filter
               , round
               , fromIntegral
               , (*)
               , (+)
               , (-)
               , (/)
               , (.)
               , ($)
               , (==)
               , (++)
               , (!!)
               )

import Main.Perlenspiel ( psBeadColor
                        , psBorderColor
                        , psRadius
                        , psAll
                        )

import GameLogic.Move ( Facing(..)
                      )
import GameLogic.Types ( GridBead(..)
                       )
import GameLogic.GameMap ( GameMap(..) )
import GameLogic.Grid ( gridDimensions )
import GameLogic.State ( GameState(..)
                       , gameStateGameMap
                       , gameStatePlayer
                       )
import GameLogic.Player ( Player
                        , playerFacing
                        , playerPosition
                        )
import GameLogic.View ( getColorView
                      )

import Control.Lens ( (^.)
                    )

mapMInd_ :: (Int -> a -> Fay b) -> [a] -> Fay ()
mapMInd_ f = zipWithM_ f [0..]

zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

drawMap :: GameState -> Fay ()
drawMap gameState = do
    psBorderColor psAll psAll (15, 15, 15)
    psRadius psAll psAll 0
    psRadius playerZ playerY 50

    mapMInd_ (mapMInd_ . flip psBeadColor) colorView
    mapMInd_ lightRadii lights
  where
    colorView = getColorView gameState

    (_, _, maxZ) = gridDimensions . gameMapGrid $ gameState ^. gameStateGameMap
    maybeInvertZ z =  case gameState ^. gameStatePlayer ^. playerFacing of
        Positive -> z
        Negative -> maxZ - z - 1

    (playerX, playerY, playerZ') = gameState ^. gameStatePlayer ^. playerPosition
    playerZ = maybeInvertZ playerZ'

    lights = filter (\(_, (x, _, _)) -> x == playerX) $ gameMapLights (gameState ^. gameStateGameMap)
    lightRadii _ (_, (_, y, z)) = psRadius (maybeInvertZ z) y 25
