module GameLogic.Grid ( Grid(..)
                      , gridGet
                      , gridSet
                      , gridElems
                      , gridDimensions
                      ) where

import Prelude ( Int
               , Bool
               , Maybe(..)
               , length
               , concat
               , otherwise
               , zipWith
               , take
               , map
               , drop
               , head
               , (++)
               , (!!)
               , (+)
               , (.)
               , (>=)
               , (<)
               , (&&)
               , ($)
               )

import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , Position
                       )
import Data.Util.List ( mapInd
                      , replace
                      )

type Grid a = [[[a]]]

gridDimensions :: Grid a -> Position
gridDimensions grid = (sizeX, sizeY, sizeZ)
  where
    sizeX = length grid
    sizeY = length $ head grid
    sizeZ = length . head . head $ grid

validBounds :: Grid a -> GridX -> GridY -> GridZ -> Bool
validBounds grid x y z =
    x >= 0 && x < sizeX &&
    y >= 0 && y < sizeY &&
    z >= 0 && z < sizeZ
  where
    (sizeX, sizeY, sizeZ) = gridDimensions grid

gridElems :: Grid a -> [(a, Position)]
gridElems grid = concat . concat $ indexedGrid
  where
    indexedGrid = mapInd (\x -> mapInd (\y -> mapInd (\z val -> (val, (x, y, z))))) grid

gridGet :: Grid a -> Position -> Maybe a
gridGet grid (x, y, z)
    -- TODO(R): toMaybe helper function
    | validBounds grid x y z = Just $ ((grid !! x) !! y) !! z
    | otherwise = Nothing

gridSet :: Grid a -> Position -> a -> Grid a
gridSet grid (x, y, z) value = newMap
  where
    xList = grid !! x
    yList = xList !! y

    newZ = value
    newYList = replace yList z newZ
    newXList = replace xList y newYList

    newMap = replace grid x newXList
