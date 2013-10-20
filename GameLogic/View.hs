module GameLogic.View ( getView
                      ) where

import Prelude ( Maybe(..)
               , Int
               , reverse
               , zipWith
               , map
               , otherwise
               , (.)
               , ($)
               , (+)
               , (==)
               , (-)
               , (!!)
               )

import GameLogic.State ( GameState(..)
                       )
import GameLogic.GameMap ( gameMapGrid
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

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just something) = something
fromMaybe def _ = def

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

getView :: GameState -> [[Color]]
getView (GameState player gameMap) =
    mapInd (mapInd . beadColor) viewSection
  where
    grid = gameMapGrid gameMap
    positiveFacing = playerGetFacing player == Positive

    (maxX, _, maxZ) = gridDimensions grid
    invertZ z = maxZ - z - 1

    (playerX, playerY, playerZ') = playerGetPosition player
    playerZ = if positiveFacing
              then playerZ'
              else invertZ playerZ'

    viewSection' = grid !! playerX
    viewSection = if positiveFacing
                  then viewSection'
                  else map reverse viewSection'

    distanceToWall :: GridY -> GridZ -> Int
    distanceToWall y z' =
        let z = if positiveFacing
                then z'
                else invertZ z'

            -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid x y z) [(-1)..maxX]

            delta = if positiveFacing
                    then (+ 1)
                    else (+ (-1))

            distance :: Int -> Int -> Int
            distance dist index
                | xSlice !! index == Wall = dist
                | otherwise = distance (dist + 1) (delta index)

        in distance 0 $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> Color
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = PlayerColor
        | otherwise = 
            case bead of
                (Door _ _) -> DoorColor
                _ -> WallColor $ distanceToWall y z
