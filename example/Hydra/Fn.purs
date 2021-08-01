module Hydra.Fn where


import Prelude (map)

import Data.Functor (class Functor)


import Data.Tuple.Nested ((/\), type (/\))


-- x defines the type of the function argument
-- newtype Fn x = Fn { name :: String, args :: Array (String /\ x), out :: String /\ x }

newtype Fn x = Fn { name :: String, args :: Array (String /\ x) }


class ToFn a x where
    toFn :: a -> Fn x


instance toFnFunctor :: Functor Fn where
    map f (Fn { name, args }) =
        Fn { name, args : map (map f) args }
    {- map f (Fn { name, args, out }) =
        Fn { name, args : map (map f) args, out : map f out } -}


{-
fn :: forall x. String -> Array (String /\ x) -> String /\ x -> Fn x
fn name args out = Fn { name, args, out } -}


fn :: forall x. String -> Array (String /\ x) -> Fn x
fn name args = Fn { name, args }