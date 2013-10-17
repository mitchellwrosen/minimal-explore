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
import GameLogic.Types ( GridY
                       , GridZ
                       , Color(..)
                       , GridBead(..)
                       )

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

getView :: GameState -> [[Color]]
getView (GameState player grid) =
    mapInd (mapInd . beadColor) viewSection
  where
    positiveFacing = playerGetFacing player == Positive

    (maxX, _, maxZ) = gridDimensions grid
    invertZ z = maxZ - z - 1

    (playerX, playerY, playerZ') = playerGetPosition player
    playerZ
        | positiveFacing = playerZ'
        | otherwise =  invertZ playerZ'

    viewSection' = grid !! playerX
    viewSection
        | positiveFacing = viewSection'
        | otherwise = map reverse viewSection'

    distanceToWall :: GridY -> GridZ -> Int
    distanceToWall y z' =
        let z = if positiveFacing
                then z'
                else invertZ z'

            -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid x y z) [(-1)..maxX]

            delta
                | positiveFacing = (+ 1)
                | otherwise = (+ (-1))

            distance :: Int -> Int -> Int
            distance dist index
                | xSlice !! index == Wall = dist
                | otherwise = distance (dist + 1) (delta index)

        in distance 0 $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> Color
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = PlayerColor
        | otherwise = WallColor $ distanceToWall y z
