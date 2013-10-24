module GameLogic.View.Internal ( getColorView
                               , getView
                               , phongLighting
                               , lightIntensity
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
               , div
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
               , (/=)
               , (<=)
               , (>)
               , (-)
               , (!!)
               )
import GameLogic.View.Light ( phongLighting
                            , beadDiffuse
                            , lightIntensity
                            )

import GameLogic.State ( GameState(..)
                       )
import GameLogic.GameMap ( gameMapGrid
                         , gameMapAmbientLight
                         , GameMap(..)
                         )
import GameLogic.Move ( Facing(..)
                      , Position(..)
                      )
import GameLogic.Player ( playerGetFacing
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
                       , Light(..)
                       , GridBead(..)
                       )
import GameLogic.Color ( BeadColor(..)
                       , Color(..)
                       )

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

getColorView :: GameState -> [[Color]]
getColorView gameState = map (map calculateBeadColor) $ getView gameState
  where
    maxLight = gameMapAmbientLight $ gameStateGameMap gameState

    fadeValue :: Int -> Int
    fadeValue 0 = 0
    fadeValue 1 = round $ fromIntegral maxLight * 64 / 255
    fadeValue 2 = round $ fromIntegral maxLight * 128 / 255
    fadeValue 3 = round $ fromIntegral maxLight * 192 / 255
    fadeValue _ = maxLight

    calculateBeadColor :: (BeadColor, [(Light, Int)]) -> Color
    calculateBeadColor (color@(EmptyColor), lights) =
        phongLighting (beadDiffuse color) (maxLight, maxLight, maxLight) lights
    calculateBeadColor (color@(PlayerColor), lights) =
        phongLighting (beadDiffuse color) (maxLight, 0, 0) lights
    calculateBeadColor (color@(WallColor dist), lights) =
        phongLighting (beadDiffuse color) (fadeValue dist, fadeValue dist, fadeValue dist) lights
    calculateBeadColor (color@(DoorColor dist), lights) =
        phongLighting (beadDiffuse color) (maxLight, maxLight, maxLight) lights
    calculateBeadColor (LightColor color, _) = color

getBeadView :: GameState -> [[GridBead]]
getBeadView (GameState player gameMap) = viewSection
  where
    grid = (gameMapGrid gameMap)
    (playerX, _, _) = playerGetPosition player
    positiveFacing = playerGetFacing player == Positive

    viewSection' = grid !! playerX
    viewSection = if positiveFacing
                  then viewSection'
                  else map reverse viewSection'

getView :: GameState -> [[(BeadColor, [(Light, Int)])]]
getView gameState@(GameState player gameMap) =
    mapInd (mapInd . beadColor) (getBeadView gameState)
  where
    grid = gameMapGrid gameMap
    positiveFacing = playerGetFacing player == Positive

    (maxX, _, maxZ) = gridDimensions grid
    invertZ z = maxZ - z - 1
    invertZIfNegative z = if positiveFacing
                          then z
                          else invertZ z

    (playerX, playerY, playerZ') = playerGetPosition player
    playerZ = invertZIfNegative playerZ'

    nearbyLightBeads :: GridY -> GridZ -> [(Light, Int)]
    nearbyLightBeads y z' = nearbyLightBeads'
      where
        z = invertZIfNegative z'
        lightBeads = gameMapLights gameMap

        lightsWithDistance :: [(Light, Int)]
        lightsWithDistance = map lightWithDistance lightBeads
          where
            lightWithDistance (light, lightPos) = (light, distance lightPos (playerX, y, z))

        nearbyFilter (light, dist) = dist <= lightRadius light
        nearbyLightBeads' = filter nearbyFilter lightsWithDistance

        distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
        distance (x1, y1, z1) (x2, y2, z2) =
            let subSqr a b = (fromIntegral (a - b))*(fromIntegral (a - b))
            in  round . sqrt $ subSqr x2 x1 + subSqr y2 y1 + subSqr z2 z1

    xDistanceToBead :: GridY -> GridZ -> (Int, GridBead)
    xDistanceToBead y z' =
        let z = invertZIfNegative z'

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

    fromList :: [a] -> Maybe a
    fromList (a:xs) = Just a
    fromList [] = Nothing

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

    lightAt :: GridY -> GridZ -> Maybe (Light, Position)
    lightAt y z =
        let lights = filter (\(_, pos) -> pos == (playerX, y, z)) $ gameMapLights gameMap
        in  fromList lights

    beadColor :: GridY -> GridZ -> GridBead -> (BeadColor, [(Light, Int)])
    beadColor y z bead
        | (y, z) == (playerY, playerZ) = (PlayerColor, nearbyLightBeads y z)
        | isJust $ lightAt y (invertZIfNegative z) =
            let Just (Light _ color, _) = lightAt y (invertZIfNegative z)
            in  (LightColor color, [])
        | otherwise =
            let (dist, bead) = xDistanceToBead y z
            in  case bead of
                    Wall -> (WallColor dist, nearbyLightBeads y z)
                    (DoorBead _) -> (DoorColor dist, nearbyLightBeads y z)
    
