module Data.Maybe where

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just something) = something
fromMaybe def _ = def

fromJust :: Maybe a -> a
fromJust (Just a) = a

containsNothing :: [Maybe a] -> Bool
containsNothing = isNothing . foldr1 intoMaybe

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

returnMaybe :: a -> Maybe a
returnMaybe = Just

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing f = Nothing
bindMaybe (Just a) f = f a

intoMaybe :: Maybe a -> Maybe b -> Maybe b
intoMaybe Nothing _ = Nothing
intoMaybe _ b = b
