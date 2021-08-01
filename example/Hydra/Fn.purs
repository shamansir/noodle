module Hydra.Fn where


import Prelude (map)

import Hydra (Hydra(..))

import Data.Functor (class Functor)

import Data.Tuple.Nested ((/\), type (/\))


-- x defines the type of the function argument
newtype Fn x = Fn { name :: String, args :: Array (String /\ x), out :: String /\ x }


class ToFn a x where
    toFn :: a -> Fn x


instance toFnFunctor :: Functor Fn where
    map f (Fn { name, args, out }) =
        Fn { name, args : map (map f) args, out : map f out }
