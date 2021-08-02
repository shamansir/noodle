module Hydra.Fn where


import Prelude (map, (==), (<$>), ($), otherwise, const, flip)

import Data.Functor (class Functor)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)

import Data.Vec (Vec, (!!), (+>))
import Data.Vec (fromArray, toArray, zipWithE, singleton, empty) as Vec
import Data.Typelevel.Num.Reps (D0, D1, D2, D3, D4, D5, D6, d0, d1, d2, d3, d4, d5)


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
    | Unknown


instance functorArgs :: Functor Args where
    map f (Arg0 v) = Arg0 $ map f v
    map f (Arg1 v) = Arg1 $ map f v
    map f (Arg2 v) = Arg2 $ map f v
    map f (Arg3 v) = Arg3 $ map f v
    map f (Arg4 v) = Arg4 $ map f v
    map f (Arg5 v) = Arg5 $ map f v
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


addNames :: forall x. Fn x -> Fn (String /\ x)
addNames (Fn { name, args }) =
    Fn { name, args : arrayToArgs $ h1 <$> argsToArray args }
    where h1 (arg /\ val) = (arg /\ arg /\ val)


{-
fn :: forall x. String -> Array (String /\ x) -> String /\ x -> Fn x
fn name args out = Fn { name, args, out } -}


getName :: forall x. Fn x -> String
getName (Fn { name }) = name


getArgs :: forall x. Fn x -> Array (String /\ x)
getArgs (Fn { args }) = argsToArray args


argsToArray :: forall x. Args x -> Array x
argsToArray (Arg0 v) = Vec.toArray v
argsToArray (Arg1 v) = Vec.toArray v
argsToArray (Arg2 v) = Vec.toArray v
argsToArray (Arg3 v) = Vec.toArray v
argsToArray (Arg4 v) = Vec.toArray v
argsToArray (Arg5 v) = Vec.toArray v
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
        helper _ = Unknown


        --helper' f arr = fromMaybe Unknown (f <$> Vec.fromArray arr)

applyVec0 :: forall x z. z -> Vec D0 x -> z
applyVec0 z _ = z


applyVec1 :: forall x z.  (x -> z) -> Vec D1 x -> z
applyVec1 f v = f (v !! d0)


applyVec2 :: forall x z.  (x -> x -> z) -> Vec D2 x -> z
applyVec2 f v = f (v !! d0) (v !! d1)


applyVec3 :: forall x z.  (x -> x -> x -> z) -> Vec D3 x -> z
applyVec3 f v = f (v !! d0) (v !! d1) (v !! d2)


applyVec4 :: forall x z.  (x -> x -> x -> x -> z) -> Vec D4 x -> z
applyVec4 f v = f (v !! d0) (v !! d1) (v !! d2) (v !! d3)


applyVec5 :: forall x z.  (x -> x -> x -> x -> x -> z) -> Vec D5 x -> z
applyVec5 f v = f (v !! d0) (v !! d1) (v !! d2) (v !! d3) (v !! d4)


applyArgs0 :: forall x z. z -> Args x -> Maybe z
applyArgs0 z (Arg0 v) = Just $ applyVec0 z v
applyArgs0 _ _ = Nothing


applyArgs1 :: forall x z. (x -> z) -> Args x -> Maybe z
applyArgs1 f (Arg1 v) = Just $ applyVec1 f v
applyArgs1 _ _ = Nothing


applyArgs2 :: forall x z. (x -> x -> z) -> Args x -> Maybe z
applyArgs2 f (Arg2 v) = Just $ applyVec2 f v
applyArgs2 _ _ = Nothing


applyArgs3 :: forall x z. (x -> x -> x -> z) -> Args x -> Maybe z
applyArgs3 f (Arg3 v) = Just $ applyVec3 f v
applyArgs3 _ _ = Nothing


applyArgs4 :: forall x z. (x -> x -> x -> x -> z) -> Args x -> Maybe z
applyArgs4 f (Arg4 v) = Just $ applyVec4 f v
applyArgs4 _ _ = Nothing


applyArgs5 :: forall x z. (x -> x -> x -> x -> x -> z) -> Args x -> Maybe z
applyArgs5 f (Arg5 v) = Just $ applyVec5 f v
applyArgs5 _ _ = Nothing


applyFn0 :: forall x z. z -> Fn x -> Maybe z
applyFn0 v (Fn { args }) = applyArgs0 v $ snd <$> args


applyFn1 :: forall x z. (x -> z) -> Fn x -> Maybe z
applyFn1 f (Fn { args }) = applyArgs1 f $ snd <$> args


applyFn2 :: forall x z. (x -> x -> z) -> Fn x -> Maybe z
applyFn2 f (Fn { args }) = applyArgs2 f $ snd <$> args


applyFn3 :: forall x z. (x -> x -> x -> z) -> Fn x -> Maybe z
applyFn3 f (Fn { args }) = applyArgs3 f $ snd <$> args


applyFn4 :: forall x z. (x -> x -> x -> x -> z) -> Fn x -> Maybe z
applyFn4 f (Fn { args }) = applyArgs4 f $ snd <$> args


applyFn5 :: forall x z. (x -> x -> x -> x -> x -> z) -> Fn x -> Maybe z
applyFn5 f (Fn { args }) = applyArgs5 f $ snd <$> args


tryFn1 :: forall x z. (z -> Maybe x) -> x -> (x -> z) -> (z -> z)
tryFn1 toX v1 fnToZ = tryFn1' toX (Vec.singleton v1) $ applyVec1 fnToZ


tryFn2 :: forall x z. (z -> Maybe x) -> x /\ x -> (x -> x -> z) -> (z -> z -> z)
tryFn2 toX (v1 /\ v2) fnToZ = tryFn2' toX (v1 +> v2 +> Vec.empty) $ applyVec2 fnToZ


tryFn3 :: forall x z. (z -> Maybe x) -> x /\ x /\ x -> (x -> x -> x -> z) -> (z -> z -> z -> z)
tryFn3 toX (v1 /\ v2 /\ v3) fnToZ = tryFn3' toX (v1 +> v2 +> v3 +> Vec.empty) $ applyVec3 fnToZ


tryFn4 :: forall x z. (z -> Maybe x) -> x /\ x /\ x /\ x -> (x -> x -> x -> x -> z) -> (z -> z -> z -> z -> z)
tryFn4 toX (v1 /\ v2 /\ v3 /\ v4) fnToZ = tryFn4' toX (v1 +> v2 +> v3 +> v4 +> Vec.empty) $ applyVec4 fnToZ


tryFn5 :: forall x z. (z -> Maybe x) -> x /\ x /\ x /\ x /\ x -> (x -> x -> x -> x -> x -> z) -> (z -> z -> z -> z -> z -> z)
tryFn5 toX (v1 /\ v2 /\ v3 /\ v4 /\ v5 ) fnToZ = tryFn5' toX (v1 +> v2 +> v3 +> v4 +> v5 +> Vec.empty) $ applyVec5 fnToZ


tryFn1' :: forall x z. (z -> Maybe x) -> Vec D1 x -> (Vec D1 x -> z) -> (z -> z)
tryFn1' toX defaults fnToZ p1 =
    fnToZ $ Vec.zipWithE (flip fromMaybe) (toX <$> Vec.singleton p1) defaults


tryFn2' :: forall x z. (z -> Maybe x) -> Vec D2 x -> (Vec D2 x -> z) -> (z -> z -> z)
tryFn2' toX defaults fnToZ p1 p2 =
    fnToZ $ Vec.zipWithE (flip fromMaybe) (toX <$> p1 +> p2 +> Vec.empty) defaults


tryFn3' :: forall x z. (z -> Maybe x) -> Vec D3 x -> (Vec D3 x -> z) -> (z -> z -> z -> z)
tryFn3' toX defaults fnToZ p1 p2 p3 =
    fnToZ $ Vec.zipWithE (flip fromMaybe) (toX <$> p1 +> p2 +> p3 +> Vec.empty) defaults


tryFn4' :: forall x z. (z -> Maybe x) -> Vec D4 x -> (Vec D4 x -> z) -> (z -> z -> z -> z -> z)
tryFn4' toX defaults fnToZ p1 p2 p3 p4 =
    fnToZ $ Vec.zipWithE (flip fromMaybe) (toX <$> p1 +> p2 +> p3 +> p4 +> Vec.empty) defaults


tryFn5' :: forall x z. (z -> Maybe x) -> Vec D5 x -> (Vec D5 x -> z) -> (z -> z -> z -> z -> z -> z)
tryFn5' toX defaults fnToZ p1 p2 p3 p4 p5 =
    fnToZ $ Vec.zipWithE (flip fromMaybe) (toX <$> p1 +> p2 +> p3 +> p4 +> p5 +> Vec.empty) defaults
