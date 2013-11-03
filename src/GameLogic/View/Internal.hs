module GameLogic.View.Internal ( getColorView
                               , getColorViewAt
                               , getView
                               , getBeadView
                               , phongLighting
                               , lightIntensity
                               ) where

import Prelude

import GameLogic.View.Light ( phongLighting
                            , beadDiffuse
                            , lightIntensity
                            )

import GameLogic.GameMap ( gameMapGrid
                         , gameMapLights
                         , gameMapAmbientLight
                         )
import GameLogic.Player ( playerPosition
                        , playerFacing
                        )
import GameLogic.Grid ( gridDimensions
                      , gridGet
                      )
import GameLogic.Types ( GridY
                       , GridZ
                       , posX
                       , posZ
                       , Light(..)
                       , GridBead(..)
                       , BeadColor(..)
                       , Color
                       , Position
                       , Facing(..)
                       , GameState
                       , gameStateGameMap
                       , gameStatePlayer
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

correctZ :: GameState -> GridZ -> GridZ
correctZ gameState z =
    let maxZ = (gridDimensions $ gameState^.gameStateGameMap^.gameMapGrid)^.posZ
    in  if isPositiveFacing gameState
        then z
        else maxZ - z - 1

getColorViewAt :: GameState -> Position -> Color
getColorViewAt gameState (_, y, z) =
    ((getColorView gameState) !! y) !! (correctZ gameState z)

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

            xDistance' dist index =
                case xSlice !! index of
                    Wall -> (dist, Wall)
                    door@(DoorBead _) -> (dist, door)
                    _ -> xDistance' (dist + 1) (delta index)

            xDistance :: Int -> (Int, GridBead)
            xDistance = xDistance' 0

        in  xDistance $ playerX + 1

    beadColor :: GridY -> GridZ -> GridBead -> (BeadColor, [(Light, Int)])
    beadColor beadY beadZ' _
        | (beadY, beadZ) == (playerY, playerZ) = (PlayerColor, nearbyLightBeads')
        | lightIsAt beadY beadZ = lightBeadColor
        | otherwise     = staticBeadColor
      where
        lightIsAt y = isJust . lightAt y
        nearbyLightBeads' = nearbyLightBeads beadY beadZ

        lightAt :: GridY -> GridZ -> Maybe (Light, Position)
        lightAt y z = fromList lights
          where
            lights = filter (\(_, pos) -> pos == (playerX, y, z)) $ gameMap^.gameMapLights

        lightBeadColor =
            let Just (Light _ color, _) = lightAt beadY beadZ
            in  (LightColor color, [])

        staticBeadColor = case bead of
            Wall         -> (WallColor dist, nearbyLightBeads')
            (DoorBead _) -> (DoorColor dist, nearbyLightBeads')
            _ -> error "Does not calculate BeadColors of LightBeads or Emptys"
          where
            (dist, bead) = xDistanceToBead beadY beadZ

        beadZ = correctZ gameState beadZ'
