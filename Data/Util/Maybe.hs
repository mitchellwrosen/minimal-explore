module Data.Util.Maybe ( fromMaybe
                       , toMaybe
                       ) where

import Prelude ( Maybe(..)
               , Bool(..)
               , flip
               , maybe
               , id
               )

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a
