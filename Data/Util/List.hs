module Data.Util.List ( findFirst
                      , filterMap
                      , mapInd
                      , replace
                      ) where

import Prelude ( filter
               , Bool
               , Maybe
               , Int
               , zipWith
               , foldr
               , head
               , maybe
               , take
               , drop
               , (+)
               , (++)
               )

mapInd :: (Int -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

findFirst :: (a -> Bool) -> [a] -> a
findFirst filt list = head (filter filt list)

replace :: [a] -> Int -> a -> [a]
replace list index val =
    take index list ++ val : drop (index + 1) list

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr foldFunc []
  where
    foldFunc a acc = maybe acc (:acc) (f a)

