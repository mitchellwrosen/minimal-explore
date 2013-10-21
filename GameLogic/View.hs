module GameLogic.View ( getView
                      ) where

import Prelude ( Maybe(..)
               , Int
               , Bool(..)
               , reverse
               , zipWith
               , map
               , otherwise
               , filter
               , round
               , sqrt
               , fromIntegral
               , undefined
               , (.)
               , (*)
               , ($)
               , (+)
               , (==)
               , (<=)
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
                      , gridElems
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

getView :: GameState -> [[(Color, [(GridBead, Int)])]]
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

    nearbyLightBeads :: GridY -> GridZ -> [(GridBead, Int)]
    nearbyLightBeads y z' = nearbyLightBeads'
      where
        z = if positiveFacing
            then z'
            else invertZ z'
        lightBeadFilter ((Light _ _), _) = True
        lightBeadFilter _ = False
        lightBeads = filter lightBeadFilter (gridElems grid)

        lightBeadsWithDistance = map lightWithDistance lightBeads
          where
            lightWithDistance (light@(Light _ _), lightPos) =
                (light, distance lightPos (playerX, y, z))

        nearbyFilter ((Light radius _), dist) = dist <= radius
        nearbyFilter _ = False
        nearbyLightBeads' = filter nearbyFilter lightBeadsWithDistance

        distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
        distance (x1, y1, z1) (x2, y2, z2) =
            let subSqr a b = (fromIntegral (a - b))*(fromIntegral (a - b))
            in  round . sqrt $ subSqr x2 x1 + subSqr y2 y1 + subSqr z2 z1

    distanceToBead :: GridY -> GridZ -> (Int, GridBead)
    distanceToBead y z' =
        let z = if positiveFacing
                then z'
                else invertZ z'

            -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid x y z) [(-1)..maxX]

            delta = if positiveFacing
                    then (+ 1)
                    else (+ (-1))

            distance :: Int -> Int -> (Int, GridBead)
            distance dist index =
                case xSlice !! index of
                    Wall -> (dist, Wall)
                    door@(Door _ _) -> (dist, door)
                    _ -> distance (dist + 1) (delta index)
        in  distance 0 $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> (Color, [(GridBead, Int)])
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = (PlayerColor, nearbyLightBeads y z)
        | otherwise =
            let (dist, bead) = distanceToBead y z
            in  case bead of
                    Wall -> (WallColor dist, nearbyLightBeads y z)
                    (Door _ _) -> (DoorColor dist, nearbyLightBeads y z)
