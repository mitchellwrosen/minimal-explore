module Main.Ref ( modifyRef
                , newRef
                , writeRef
                , readRef
                ) where

import Prelude ( Fay
               , (>>=)
               , (.)
               )
import FFI ( ffi
           )

-- | A mutable reference like IORef.
data Ref a

-- | Make a new mutable reference.
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

-- | Replace the value in the mutable reference.
writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

-- | Get the referred value from the mutable value.
readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

modifyRef :: Ref a -> (a -> a) -> Fay ()
modifyRef ref f =
    readRef ref >>= writeRef ref . f
