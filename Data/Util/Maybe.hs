module Data.Util.Maybe ( fromMaybe
                       ) where

import Prelude ( Maybe(..)
               , flip
               , maybe
               , id
               )

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

