module GameLogic.Grid ( Grid(..)
                      , gridGet
                      , gridSet
                      , gridElems
                      , gridDimensions
                      , replace
                      ) where

import Prelude ( Int
               , Bool
               , Maybe(..)
               , length
               , concat
               , otherwise
               , take
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
                       )

type Grid a = [[[a]]]

gridDimensions :: Grid a -> (GridX, GridY, GridZ)
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

gridElems :: Grid a -> [a]
gridElems = concat . concat

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

replace :: [a] -> Int -> a -> [a]
replace list index val =
    take index list ++ val : drop (index + 1) list
