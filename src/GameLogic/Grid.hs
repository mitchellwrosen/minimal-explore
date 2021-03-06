module GameLogic.Grid ( Grid
                      , gridGet
                      , gridSet
                      , gridElems
                      , gridDimensions
                      ) where

import Prelude ( Int
               , Bool
               , Maybe(..)
               , length
               , replicate
               , concat
               , otherwise
               , zipWith
               , take
               , foldr
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

import GameLogic.Types ( Position
                       , Grid
                       )
import Data.Util.Maybe ( toMaybe )
import Data.Util.List ( mapInd
                      , replace
                      )

gridDimensions :: Grid a -> Position
gridDimensions grid = (sizeX, sizeY, sizeZ)
  where
    sizeX = length $ grid
    sizeY = length . head $ grid
    sizeZ = length . head . head $ grid

validPosition :: Grid a -> Position -> Bool
validPosition grid (x, y, z) =
    x `isBoundedBy` sizeX &&
    y `isBoundedBy` sizeY &&
    z `isBoundedBy` sizeZ
  where
    (sizeX, sizeY, sizeZ) = gridDimensions grid
    isBoundedBy val hi = val >= 0 && val < hi

gridElems :: Grid a -> [(a, Position)]
gridElems grid = concat . concat $ indexedGrid
  where
    indexedGrid = mapInd (\x -> mapInd (\y -> mapInd (\z val -> (val, (x, y, z))))) grid

gridGet :: Grid a -> Position -> Maybe a
gridGet grid position@(x, y, z) =
    toMaybe (validPosition grid position) $ ((grid !! x) !! y) !! z

gridSet :: Grid a -> Position -> a -> Grid a
gridSet grid (x, y, z) value = newMap
  where
    xList = grid !! x
    yList = xList !! y

    newZ = value
    newYList = replace yList z newZ
    newXList = replace xList y newYList

    newMap = replace grid x newXList
