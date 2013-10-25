module Data.Util.Math ( distance ) where

import Prelude ( fromIntegral
               , round
               , sqrt
               , (-)
               , (*)
               , (+)
               , ($)
               , (.)
               , Int
               )

type Position = (Int, Int, Int)
distance :: Position -> Position -> Int
distance (x1, y1, z1) (x2, y2, z2) =
    let subSqr a b = (fromIntegral (a - b))*(fromIntegral (a - b))
    in  round . sqrt $ subSqr x2 x1 + subSqr y2 y1 + subSqr z2 z1
