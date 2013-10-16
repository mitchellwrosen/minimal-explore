module GameLogic.View ( getView
                      ) where

import Data.Maybe ( fromMaybe
                  )

import GameLogic.State ( GameState(..)
                       )
import GameLogic.Player ( Facing(..)
                        , playerGetFacing
                        , playerGetPosition
                        )
import GameLogic.Grid ( Grid(..)
                      , gridDimensions
                      , gridGet
                      )
import Main.Types ( GridY
                  , GridZ
                  , Color(..)
                  , GridBead(..)
                  )

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f l = zipWith f [0..] l

getView :: GameState -> [[Color]]
getView (GameState player grid) =
    mapInd (mapInd . beadColor) viewSection
  where
    playerFacing = playerGetFacing player

    (maxX, _, maxZ) = gridDimensions grid
    invertZ z = maxZ - z - 1

    (playerX, playerY, playerZ') = playerGetPosition player
    playerZ
        | playerFacing == Positive = playerZ'
        | otherwise =  invertZ playerZ'

    viewSection' = grid !! playerX
    viewSection
        | playerFacing == Positive = viewSection'
        | otherwise = map reverse viewSection'

    distanceToWall :: GridY -> GridZ -> Int
    distanceToWall y z =
        let -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid x y z) [(-1)..maxX]

            delta
                | playerFacing == Positive = (+ 1)
                | otherwise = (+ (-1))

            distance :: Int -> Int -> Int
            distance dist index
                | xSlice !! index == Wall = dist
                | otherwise = distance (dist + 1) (delta index)

        in distance 0 $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> Color
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = PlayerColor
        | playerFacing == Positive = WallColor $ distanceToWall y z
        | playerFacing == Negative = WallColor $ distanceToWall y (invertZ z)
