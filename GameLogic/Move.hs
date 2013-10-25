module GameLogic.Move ( moveForward
                      , moveUp
                      , moveDown
                      , moveLeft
                      , moveRight
                      , Move(..)
                      ) where

import Prelude ( Show
               , Eq
               , (+)
               , (-)
               , (==)
               , subtract
               )

import GameLogic.Types ( GridX
                       , GridY
                       , GridZ
                       , Facing(..)
                       , Position(..)
                       )

type Move = Facing -> Position -> Position

moveForward :: Facing -> Position -> Position
moveForward facing (x, y, z) = (delta x, y, z)
  where
    delta = if facing == Positive
            then (+ 1)
            else subtract 1

moveUp :: Facing -> Position -> Position
moveUp _ (x, y, z) = (x, y - 1, z)

moveDown :: Facing -> Position -> Position
moveDown _ (x, y, z) = (x, y + 1, z)

moveLeft :: Facing -> Position -> Position
moveLeft facing (x, y, z) = (x, y, delta z)
  where
    delta = if facing == Positive
            then subtract 1
            else (+ 1)

moveRight :: Facing -> Position -> Position
moveRight facing (x, y, z) = (x, y, delta z)
  where
    delta = if facing == Positive
            then (+ 1)
            else subtract 1
