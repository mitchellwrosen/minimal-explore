module GameLogic.View.Internal ( getColorView
                               , getView
                               , getBeadView
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
               , subtract
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

import GameLogic.State ( GameState
                       , gameStateGameMap
                       , gameStatePlayer
                       )
import GameLogic.GameMap ( gameMapGrid
                         , gameMapLights
                         , gameMapAmbientLight
                         , GameMap
                         )
import GameLogic.Player ( Player
                        , playerPosition
                        , playerFacing
                        )
import GameLogic.Grid ( Grid(..)
                      , gridDimensions
                      , gridGet
                      , gridElems
                      )
import GameLogic.Types ( GridY
                       , GridZ
                       , posX
                       , Door(..)
                       , Light(..)
                       , GridBead(..)
                       , BeadColor(..)
                       , Color(..)
                       , Position(..)
                       , Facing(..)
                       )
import GameLogic.Color ( ambientColor )

import Control.Lens ( (^.)
                    )
import Data.Util.Maybe ( fromMaybe
                       , fromList
                       , isJust
                       )
import Data.Util.List ( mapInd )
import Data.Util.Math ( distance )

getColorView :: GameState -> [[Color]]
getColorView gameState = map (map calculateBeadColor) $ getView gameState
  where
    maxLight = gameState^.gameStateGameMap^.gameMapAmbientLight

    calculateBeadColor :: (BeadColor, [(Light, Int)]) -> Color
    calculateBeadColor (LightColor color, _) = color
    calculateBeadColor (color, lights) =
        phongLighting (beadDiffuse color) (ambientColor maxLight color) lights

isPositiveFacing :: GameState -> Bool
isPositiveFacing gameState =
    gameState^.gameStatePlayer^.playerFacing == Positive

getBeadView :: GameState -> [[GridBead]]
getBeadView gameState = viewSection
  where
    grid = gameState^.gameStateGameMap^.gameMapGrid
    playerX = gameState^.gameStatePlayer^.playerPosition^.posX

    viewSection' = grid !! playerX
    viewSection = if isPositiveFacing gameState
                  then viewSection'
                  else map reverse viewSection'

getView :: GameState -> [[(BeadColor, [(Light, Int)])]]
getView gameState =
    mapInd (mapInd . beadColor) (getBeadView gameState)
  where
    gameMap = gameState^.gameStateGameMap
    grid = gameMap^.gameMapGrid

    (maxX, _, maxZ) = gridDimensions grid

    (playerX, playerY, playerZ) = gameState^.gameStatePlayer^.playerPosition

    nearbyLightBeads :: GridY -> GridZ -> [(Light, Int)]
    nearbyLightBeads y z = filter nearbyFilter lightsWithDistance
      where
        lightBeads = gameMap^.gameMapLights

        lightsWithDistance :: [(Light, Int)]
        lightsWithDistance = map lightWithDistance lightBeads
          where
            lightWithDistance (light, lightPos) = (light, distance lightPos (playerX, y, z))

        nearbyFilter (light, dist) = dist <= lightRadius light

    xDistanceToBead :: GridY -> GridZ -> (Int, GridBead)
    xDistanceToBead y z =
        let -- x slice with the given yz values
            xSlice :: [GridBead]
            xSlice = map (\x -> fromMaybe Wall $ gridGet grid (x, y, z)) [(-1)..maxX]

            delta = if isPositiveFacing gameState
                    then (+ 1)
                    else subtract 1

            distance' dist index =
                case xSlice !! index of
                    Wall -> (dist, Wall)
                    door@(DoorBead _) -> (dist, door)
                    _ -> distance' (dist + 1) (delta index)

            distance :: Int -> (Int, GridBead)
            distance = distance' 0

        in  distance $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> (BeadColor, [(Light, Int)])
    beadColor y z' bead
        | (y, z) == (playerY, playerZ) = (PlayerColor, nearbyLightBeads')
        | lightIsAt y z = lightBeadColor
        | otherwise     = staticBeadColor
      where
        lightIsAt y = isJust . lightAt y
        nearbyLightBeads' = nearbyLightBeads y z

        lightAt :: GridY -> GridZ -> Maybe (Light, Position)
        lightAt y z = fromList lights
          where
            lights = filter (\(_, pos) -> pos == (playerX, y, z)) $ gameMap^.gameMapLights

        lightBeadColor =
            let Just (Light _ color, _) = lightAt y z
            in  (LightColor color, [])

        staticBeadColor = case bead of
            Wall         -> (WallColor dist, nearbyLightBeads')
            (DoorBead _) -> (DoorColor dist, nearbyLightBeads')
          where
            (dist, bead) = xDistanceToBead y z

        z = if isPositiveFacing gameState
            then z'
            else maxZ - z' - 1
