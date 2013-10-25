module Data.Util.Maybe ( fromMaybe
                       , toMaybe
                       , fromList
                       , isJust
                       ) where

import Prelude ( Maybe(..)
               , Bool(..)
               , flip
               , maybe
               , id
               )

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

fromList :: [a] -> Maybe a
fromList (a:xs) = Just a
fromList [] = Nothing

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a
