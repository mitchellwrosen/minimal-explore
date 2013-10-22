module GameLogic.View.Internal ( getColorView
                               , getView
                               , multLights
                               , lightIntensity
                               , getR
                               , getG
                               , getB
                               ) where

import Prelude ( Maybe(..)
               , Int
               , Double
               , Bool(..)
               , reverse
               , zipWith
               , maybe
               , id
               , snd
               , foldr
               , flip
               , map
               , otherwise
               , filter
               , round
               , sqrt
               , fromIntegral
               , replicate
               , undefined
               , (.)
               , (/)
               , (*)
               , ($)
               , (+)
               , (++)
               , (==)
               , (<=)
               , (>)
               , (-)
               , (!!)
               )
import GameLogic.View.Light ( multLights
                            , beadDiffuse
                            , lightIntensity
                            , getR
                            , getG
                            , getB
                            )

import GameLogic.State ( GameState(..)
                       )
import GameLogic.GameMap ( gameMapGrid
                         , gameMapAmbientLight
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
                       , Door(..)
                       , Color(..)
                       , Light(..)
                       , GridBead(..)
                       )

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

getColorView :: GameState -> [[[Int]]]
getColorView gameState = map (map (calculateBeadColor)) $ getView gameState
  where
    maxLight = gameMapAmbientLight $ gameStateGameMap gameState

    fadeValue :: Int -> Int
    fadeValue 0 = 0
    fadeValue 1 = round $ fromIntegral maxLight * 64 / 255
    fadeValue 2 = round $ fromIntegral maxLight * 128 / 255
    fadeValue 3 = round $ fromIntegral maxLight * 192 / 255
    fadeValue _ = maxLight

    calculateBeadColor :: (Color, [(Light, Int)]) -> [Int]
    calculateBeadColor (color@(EmptyColor), lights) =
        multLights (beadDiffuse color) [maxLight, maxLight, maxLight] lights
    calculateBeadColor (color@(PlayerColor), lights) =
        multLights (beadDiffuse color) [maxLight, 0, 0] lights
    calculateBeadColor (color@(WallColor dist), lights) =
        multLights (beadDiffuse color) (replicate 3 $ fadeValue dist) lights
    calculateBeadColor (color@(DoorColor dist), lights) =
        multLights (beadDiffuse color) ((replicate 2 $ fadeValue dist) ++ [maxLight]) lights


getView :: GameState -> [[(Color, [(Light, Int)])]]
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

    nearbyLightBeads :: GridY -> GridZ -> [(Light, Int)]
    nearbyLightBeads y z' = nearbyLightBeads'
      where
        z = if positiveFacing
            then z'
            else invertZ z'
        lightBeadFilter (LightBead (Light _ _), _) = True
        lightBeadFilter _ = False
        lightBeads = filter lightBeadFilter (gridElems grid)

        lightsWithDistance :: [(Light, Int)]
        lightsWithDistance = map lightWithDistance lightBeads
          where
            lightWithDistance (LightBead light@(Light _ _), lightPos) =
                (light, distance lightPos (playerX, y, z))

        nearbyFilter (light, dist) = dist <= lightRadius light
        nearbyLightBeads' = filter nearbyFilter lightsWithDistance

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
                    door@(DoorBead _) -> (dist, door)
                    _ -> distance (dist + 1) (delta index)
        in  distance 0 $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> (Color, [(Light, Int)])
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = (PlayerColor, nearbyLightBeads y z)
        | otherwise =
            let (dist, bead) = distanceToBead y z
            in  case bead of
                    Wall -> (WallColor dist, nearbyLightBeads y z)
                    (DoorBead (Door _ _)) -> (DoorColor dist, nearbyLightBeads y z)
