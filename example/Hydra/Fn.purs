module Hydra.Fn where


import Prelude (map, (==), (<$>), ($), otherwise, const)

import Data.Functor (class Functor)
import Data.Array as Array
import Data.Maybe (fromMaybe)

import Data.Vec (Vec)
import Data.Vec (fromArray, toArray) as Vec
import Data.Typelevel.Num.Reps (D0, D1, D2, D3, D4, D5, D6)


import Data.Tuple.Nested ((/\), type (/\))


-- x defines the type of the function argument
-- newtype Fn x = Fn { name :: String, args :: Array (String /\ x), out :: String /\ x }

data Args x
    = Arg0 (Vec D0 x)
    | Arg1 (Vec D1 x)
    | Arg2 (Vec D2 x)
    | Arg3 (Vec D3 x)
    | Arg4 (Vec D4 x)
    | Arg5 (Vec D5 x)
    | Arg6 (Vec D6 x)
    | Unknown


instance functorArgs :: Functor Args where
    map f (Arg0 v) = Arg0 $ map f v
    map f (Arg1 v) = Arg1 $ map f v
    map f (Arg2 v) = Arg2 $ map f v
    map f (Arg3 v) = Arg3 $ map f v
    map f (Arg4 v) = Arg4 $ map f v
    map f (Arg5 v) = Arg5 $ map f v
    map f (Arg6 v) = Arg6 $ map f v
    map _ Unknown = Unknown


newtype Fn x = Fn { name :: String, args :: Args (String /\ x) }


class ToFn a x where
    toFn :: a -> Fn x


instance toFnFunctor :: Functor Fn where
    map f (Fn { name, args }) =
        Fn { name, args : map (map f) args }
    {- map f (Fn { name, args, out }) =
        Fn { name, args : map (map f) args, out : map f out } -}

fn :: forall x. String -> Array (String /\ x) -> Fn x
fn name args = Fn { name, args : arrayToArgs args }


{-
fn :: forall x. String -> Array (String /\ x) -> String /\ x -> Fn x
fn name args out = Fn { name, args, out } -}


argsToArray :: forall x. Args x -> Array x
argsToArray (Arg0 v) = Vec.toArray v
argsToArray (Arg1 v) = Vec.toArray v
argsToArray (Arg2 v) = Vec.toArray v
argsToArray (Arg3 v) = Vec.toArray v
argsToArray (Arg4 v) = Vec.toArray v
argsToArray (Arg5 v) = Vec.toArray v
argsToArray (Arg6 v) = Vec.toArray v
argsToArray Unknown  = []



arrayToArgs :: forall x. Array x -> Args x
arrayToArgs arr =
    helper (Array.length arr)
    where
        --helper :: forall x. Int -> Array x -> Args x
        helper 0 = fromMaybe Unknown (Arg0 <$> Vec.fromArray arr)
        helper 1 = fromMaybe Unknown (Arg1 <$> Vec.fromArray arr)
        helper 2 = fromMaybe Unknown (Arg2 <$> Vec.fromArray arr)
        helper 3 = fromMaybe Unknown (Arg3 <$> Vec.fromArray arr)
        helper 4 = fromMaybe Unknown (Arg4 <$> Vec.fromArray arr)
        helper 5 = fromMaybe Unknown (Arg5 <$> Vec.fromArray arr)
        helper 6 = fromMaybe Unknown (Arg6 <$> Vec.fromArray arr)
        helper _ = Unknown


        --helper' f arr = fromMaybe Unknown (f <$> Vec.fromArray arr)
