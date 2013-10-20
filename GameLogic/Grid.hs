module GameLogic.Grid ( Grid(..)
                      , gridGet
                      , gridSet
                      , gridDimensions
                      , replace
                      ) where

import Prelude ( Int
               , Bool
               , Maybe(..)
               , length
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

import GameLogic.Types 
    ( Position(..)
    , posX
    , posY
    , posZ
    )

import Control.Lens ( (^.) )

type Grid a = [[[a]]]

gridDimensions :: Grid a -> Position
gridDimensions grid = Position (sizeX, sizeY, sizeZ)
  where
    sizeX = length grid
    sizeY = length $ head grid
    sizeZ = length . head . head $ grid

validBounds :: Grid a -> Position -> Bool
validBounds grid (Position (x,y,z)) =
    x >= 0 && x < sizeX &&
    y >= 0 && y < sizeY &&
    z >= 0 && z < sizeZ
  where
    Position (sizeX, sizeY, sizeZ) = gridDimensions grid

replace :: [a] -> Int -> a -> [a]
replace list index val =
    take index list ++ val : drop (index + 1) list

gridGet :: Grid a -> Position -> Maybe a
gridGet grid pos
    | validBounds grid pos = Just $ ((grid !! (pos^.posX)) !! (pos^.posY)) !! (pos^.posZ)
    | otherwise = Nothing

gridSet :: Grid a -> Position -> a -> Grid a
gridSet grid (Position (x,y,z)) value = newMap
  where
    xList = grid !! x
    yList = xList !! y

    newZ = value
    newYList = replace yList z newZ
    newXList = replace xList y newYList

    newMap = replace grid x newXList
