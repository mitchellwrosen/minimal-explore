module GameLogic.Grid where

import Main.Types

type Grid a = [[[a]]]

replace :: [a] -> Int -> a -> [a]
replace list index val =
    (take index list) ++ val:(drop (index + 1) list)

gridGet :: Grid a -> GridX -> GridY -> GridZ -> a
gridGet map x y z = ((map !! x) !! y) !! z

gridSet :: Grid a -> GridX -> GridY -> GridZ -> a -> Grid a
gridSet map x y z value = newMap
  where
    xList = map !! x
    yList = xList !! y

    newZ = value
    newYList = replace yList z newZ
    newXList = replace xList y newYList

    newMap = replace map x newXList
