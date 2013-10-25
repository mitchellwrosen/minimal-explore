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
                      )
import GameLogic.Player ( Player(..)
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
                       , BeadColor(..)
                       , Color(..)
                       , Position(..)
                       )

--TODO(R): Helper module
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

--TODO(R): Helper module
mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

getColorView :: GameState -> [[Color]]
getColorView gameState = map (map calculateBeadColor) $ getView gameState
  where
    -- TODO(R): lens
    maxLight = gameMapAmbientLight $ gameStateGameMap gameState

    -- TODO(R): only appropriate for walls
    fadeValue :: Int -> Int
    fadeValue 0 = 0
    fadeValue 1 = round $ fromIntegral maxLight * 64 / 255
    fadeValue 2 = round $ fromIntegral maxLight * 128 / 255
    fadeValue 3 = round $ fromIntegral maxLight * 192 / 255
    fadeValue _ = maxLight

    -- TODO(R): ambient colors for beads should be elsewhere
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
    (playerX, _, _) = _playerPosition player
    positiveFacing = _playerFacing player == Positive

    viewSection' = grid !! playerX
    viewSection = if positiveFacing
                  then viewSection'
                  else map reverse viewSection'

getView :: GameState -> [[(BeadColor, [(Light, Int)])]]
getView gameState@(GameState player gameMap) =
    mapInd (mapInd . beadColor) (getBeadView gameState)
  where
    --TODO(R): Only invert z once.
    grid = gameMapGrid gameMap
    positiveFacing = _playerFacing player == Positive

    (maxX, _, maxZ) = gridDimensions grid
    invertZIfNegative z = if positiveFacing
                          then z
                          else maxZ - z - 1

    (playerX, playerY, playerZ') = _playerPosition player
    playerZ = invertZIfNegative playerZ'

    nearbyLightBeads :: GridY -> GridZ -> [(Light, Int)]
    nearbyLightBeads y z' = filter nearbyFilter lightsWithDistance
      where
        z = invertZIfNegative z'
        lightBeads = gameMapLights gameMap

        lightsWithDistance :: [(Light, Int)]
        lightsWithDistance = map lightWithDistance lightBeads
          where
            lightWithDistance (light, lightPos) = (light, distance lightPos (playerX, y, z))

        nearbyFilter (light, dist) = dist <= lightRadius light

        -- TODO(R): helper module
        distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
        distance (x1, y1, z1) (x2, y2, z2) =
            let subSqr a b = (fromIntegral (a - b))*(fromIntegral (a - b))
            in  round . sqrt $ subSqr x2 x1 + subSqr y2 y1 + subSqr z2 z1

    xDistanceToBead :: GridY -> GridZ -> (Int, GridBead)
    xDistanceToBead y z' =
        let z = invertZIfNegative z'

            --TODO(R): Only invert z once.
            -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid x y z) [(-1)..maxX]

            delta = if positiveFacing
                    then (+ 1)
                    else (+ (-1))

            -- TODO(R): helper module
            distance :: Int -> Int -> (Int, GridBead)
            distance dist index =
                case xSlice !! index of
                    Wall -> (dist, Wall)
                    door@(DoorBead _) -> (dist, door)
                    _ -> distance (dist + 1) (delta index)
        in  distance 0 $ playerX + 1

    --TODO(R): helper module
    fromList :: [a] -> Maybe a
    fromList (a:xs) = Just a
    fromList [] = Nothing

    --TODO(R): helper module
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

    lightAt :: GridY -> GridZ -> Maybe (Light, Position)
    lightAt y z =
        --TODO(R): where clause
        --TODO(R): pattern match pos or cool trick
        let lights = filter (\(_, pos) -> pos == (playerX, y, z)) $ gameMapLights gameMap
        in  fromList lights

    --TODO(R): complicated, clean
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
