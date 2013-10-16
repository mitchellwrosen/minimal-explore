module GameLogic.Grid where

import Main.Types

type Grid a = [[[a]]]

gridDimensions :: Grid a -> (GridX, GridY, GridZ)
gridDimensions grid = (sizeX, sizeY, sizeZ) 
  where
    sizeX = length grid
    sizeY = length $ grid !! 0
    sizeZ = length $ (grid !! 0) !! 0

validBounds :: Grid a -> GridX -> GridY -> GridZ -> Bool
validBounds grid x y z =
    x >= 0 && x < sizeX &&
    y >= 0 && y < sizeY &&
    z >= 0 && z < sizeZ
  where
    (sizeX, sizeY, sizeZ) = gridDimensions grid

replace :: [a] -> Int -> a -> [a]
replace list index val =
    (take index list) ++ val:(drop (index + 1) list)

gridGet :: Grid a -> GridX -> GridY -> GridZ -> Maybe a
gridGet grid x y z
    | validBounds grid x y z = Just $ ((grid !! x) !! y) !! z
    | otherwise = Nothing

gridSet :: Grid a -> GridX -> GridY -> GridZ -> a -> Grid a
gridSet grid x y z value = newMap
  where
    xList = grid !! x
    yList = xList !! y

    newZ = value
    newYList = replace yList z newZ
    newXList = replace xList y newYList

    newMap = replace grid x newXList
