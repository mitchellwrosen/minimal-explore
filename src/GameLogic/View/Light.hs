module GameLogic.View.Light ( phongLighting
                            , beadDiffuse
                            , lightIntensity
                            ) where

import Prelude

import GameLogic.Types
    ( Color
    , makeColor
    , BeadColor(..)
    , Light(..)
    )
import GameLogic.Color
    ( toTriple
    )

zeroTriple :: (Double, Double, Double)
zeroTriple = (0.0, 0.0, 0.0)

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a, b, c) = (f a, f b, f c)

opTriple :: (a -> a -> a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
opTriple f (a, b, c) (x, y, z) = (f a x, f b y, f c z)

lightIntensity :: (Light, Int) -> (Double, Double, Double)
lightIntensity (Light radius color, dist)
    | dist > radius = zeroTriple
    | otherwise = (i r, i g, i b)
  where
    (r, g, b) = toTriple color
    i v = fromIntegral v / fromIntegral (dist + 1)

-- Phong Model
-- a*ka + sum [d*kd]
phongLighting :: (Double, Double, Double) -> Color -> [(Light, Int)] -> Color
phongLighting diffuseConstant ambientColor lights = color
  where
    color :: Color
    color = makeColor $ mapTriple round $ addTriple ambientComponent diffuseComponent

    ambientComponent = mapTriple fromIntegral (toTriple ambientColor)
    diffuseComponent = multTriple diffuseConstant lightContribution

    lightContribution :: (Double, Double, Double)
    lightContribution =
        foldr (addTriple . lightIntensity) zeroTriple lights

    addTriple = opTriple (+)
    multTriple = opTriple (*)

beadDiffuse :: BeadColor -> (Double, Double, Double)
beadDiffuse (PlayerColor) = (1.0, 0.1, 0.1)
beadDiffuse (WallColor _) = (0.1, 0.1, 0.1)
beadDiffuse (DoorColor _) = (1.0, 1.0, 1.0)
beadDiffuse (GateColor _) = (1.0, 1.0, 1.0)
beadDiffuse _ = error "Empty/Light Beads do not have a bead diffuse"
