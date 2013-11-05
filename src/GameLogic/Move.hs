module GameLogic.Move ( applyMove
                      , oppositeMove
                      , Move(..)
                      ) where

import Prelude ( Show
               , Eq
               , (+)
               , (-)
               , (==)
               , subtract
               )

import GameLogic.Types ( Facing(..)
                       , Position
                       )

data Move = MoveUp
          | MoveDown
          | MoveLeft
          | MoveRight
          | MoveForward
          | MoveBackward

applyMove :: Move -> Facing -> Position -> Position
applyMove (MoveUp) = moveUp
applyMove (MoveDown) = moveDown
applyMove (MoveLeft) = moveLeft
applyMove (MoveRight) = moveRight
applyMove (MoveForward) = moveForward
applyMove (MoveBackward) = moveBackward

oppositeMove :: Move -> Move
oppositeMove (MoveUp) = MoveDown
oppositeMove (MoveDown) = MoveUp
oppositeMove (MoveLeft) = MoveRight
oppositeMove (MoveRight) = MoveLeft
oppositeMove (MoveForward) = MoveBackward
oppositeMove (MoveBackward) = MoveForward

moveForward :: Facing -> Position -> Position
moveForward facing (x, y, z) = (delta x, y, z)
  where
    delta = if facing == Positive
            then (+ 1)
            else subtract 1

moveBackward :: Facing -> Position -> Position
moveBackward facing (x, y, z) = (delta x, y, z)
  where
    delta = if facing == Positive
            then subtract 1
            else (+ 1)

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
