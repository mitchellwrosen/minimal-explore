module Control.Lens where

import Prelude
    ( ($)
    , flip
    , fst
    , snd
    )

data Lens s a = Lens
    { view :: s -> a
    , set  :: a -> s -> s
    }

infixl 8 ^.
infixr 4 .~, %~
infixr 9 ~>

(^.) :: s -> Lens s a -> a
(^.) = flip view

(.~) :: Lens s a -> a -> s -> s
(.~) = set

over :: Lens s a -> (a -> a) -> s -> s
over lens f s = lens .~ newVal $ s
  where
    newVal = f (s^.lens)

(%~) :: Lens s a -> (a -> a) -> s -> s
(%~) = over

(~>) :: Lens a b -> Lens b c -> Lens a c
f ~> g = Lens (\a -> a^.f^.g) (\c a -> set f (set g c $ view f a) a)

_1 :: Lens (a,b) a
_1 = Lens fst (\a (_,b) -> (a,b))

_2 :: Lens (a,b) b
_2 = Lens snd (\b (a,_) -> (a,b))
