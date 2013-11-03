module Main.View (drawMap) where

import Prelude

import Main.Perlenspiel ( psBeadColor
                        , psBorderWidth
                        , psBorderColor
                        , psRadius
                        , psGlyph
                        , psGlyphFade
                        , psGlyphAlpha
                        , psGlyphColor
                        , psAll
                        )

import GameLogic.Types ( GridBead(..)
                       , Door(..)
                       , Gate(..)
                       , posZ
                       , Facing(..)
                       )

import GameLogic.Types.GameState
    ( GameState(..)
    , gameStateGameMap
    , gameStatePlayer
    )
import GameLogic.GameMap ( GameMap
                         , gameMapGrid
                         , gameMapLights
                         )
import GameLogic.Grid ( gridDimensions )
import GameLogic.Player ( Player
                        , playerFacing
                        , playerPosition
                        )
import GameLogic.View ( getColorView
                      , getBeadView
                      )

import Control.Lens ( (^.)
                    )

mapMInd_ :: (Int -> a -> Fay b) -> [a] -> Fay ()
mapMInd_ f = zipWithM_ f [0..]

zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

drawMap :: GameState -> Fay ()
drawMap gameState = do
    psBorderWidth psAll psAll 2
    psBorderColor psAll psAll (15, 15, 15)
    psRadius psAll psAll 0

    mapMInd_ (mapMInd_ . flip psBeadColor) colorView
    mapMInd_ (mapMInd_ . flip colorBorders) beadView
    mapMInd_ lightRadii lights
    psRadius playerZ playerY 50
  where
    colorView = getColorView gameState
    beadView = getBeadView gameState

    maxZ = gridDimensions (gameState^.gameStateGameMap^.gameMapGrid) ^. posZ
    maybeInvertZ z =  case gameState ^. gameStatePlayer ^. playerFacing of
        Positive -> z
        Negative -> maxZ - z - 1

    (playerX, playerY, playerZ') = gameState ^. gameStatePlayer ^. playerPosition
    playerZ = maybeInvertZ playerZ'

    lights = filter (\(_, (x, _, _)) -> x == playerX) $ gameState^.gameStateGameMap^.gameMapLights
    lightRadii _ (_, (_, y, z)) = psRadius (maybeInvertZ z) y 25

    colorBorders x y bead = case bead of
        DoorBead door -> do
            psBorderWidth x y 8
            psBorderColor x y (doorColor door)

            psGlyphAlpha x y 0
            psGlyphColor x y (88, 0, 178)
            psGlyph x y "Q"
            psGlyphFade x y 60

            when (doorColor door == (colorView !! y) !! x) $
                psGlyphAlpha x y 255
        GateBead gate -> do
            psBorderWidth x y 8
            psBorderColor x y (gateColor gate)
            psRadius x y 15
        _ -> return ()
