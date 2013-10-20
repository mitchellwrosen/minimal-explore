module GameLogic.Player ( Facing(..)
                        , Player(..)
                        , pPosition
                        , pFacing
                        , playerMoveUp
                        , playerMoveDown
                        , playerMoveLeft
                        , playerMoveRight
                        , playerMoveForward
                        , playerChangeDirection
                        ) where

import Prelude ( Show
               , Eq
               , ($)
               , (+)
               , (-)
               , (==)
               , subtract
               )

import GameLogic.Types
    ( Position
    , posX
    , posY
    , posZ
    )

import Control.Lens

data Facing = Positive | Negative
  deriving (Show, Eq)

data Player = Player
    { _pPosition :: Position
    , _pFacing   :: Facing
    } deriving (Show, Eq)

pPosition :: Lens Player Position
pPosition = Lens _pPosition (\a s -> s { _pPosition = a})

pFacing :: Lens Player Facing
pFacing = Lens _pFacing (\a s -> s { _pFacing = a})

playerChangeDirection :: Player -> Player
playerChangeDirection = pFacing %~ oppositeFacing
  where
    oppositeFacing Positive = Negative
    oppositeFacing Negative = Positive

playerMoveForward :: Player -> Player
playerMoveForward player = (pPosition ~> posX) %~ delta $ player
  where
    delta = if player^.pFacing == Positive
            then (+1)
            else (subtract 1)

playerMoveUp :: Player -> Player
playerMoveUp = (pPosition ~> posY) %~ (subtract 1)

playerMoveDown :: Player -> Player
playerMoveDown = (pPosition ~> posY) %~ (+1)

playerMoveLeft :: Player -> Player
playerMoveLeft player = (pPosition ~> posZ) %~ delta $ player
  where
    delta = if player^.pFacing == Positive
            then (subtract 1)
            else (+ 1)

playerMoveRight :: Player -> Player
playerMoveRight player = (pPosition ~> posZ) %~ delta $ player
  where
    delta = if player^.pFacing == Positive
            then (+ 1)
            else (subtract 1)
