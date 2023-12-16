module Tookit.Hydra.Lang.Fn where


import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (snd, uncurry) as Tuple
import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (length) as Array

import Data.Bifunctor (bimap)


-- import Tookit.Hydra.Types
-- import Tookit.Hydra.Repr.Wrap (WrapRepr)



-- TODO: use typelevel arguments counts like in https://pursuit.purescript.org/packages/purescript-fast-vect/1.1.0
--       or in example/Hydra/Fn


data Argument x = Argument ArgumentName x


data Output x = Output OutputName x


instance Show x => Show (Argument x) where
    show (Argument name v) = name <> ":" <> show v


instance Show x => Show (Output x) where
    show (Output name v) = name <> ":" <> show v


type ArgumentName = String


type OutputName = String


-- TODO: use in `ToFn` & PossinlyToFn
-- TODO: add output type
newtype Fn arg out = Fn (String /\ Array (Argument arg) /\ Array (Output out))


type FnU arg = Fn arg Unit


type FnS arg out = String /\ Array (Argument arg) /\ Array (Output out)


class ToFn arg out a where
    toFn :: a -> String /\ Array (Argument arg) /\ Array (Output out)


class PossiblyToFn arg out a where
    possiblyToFn :: a -> Maybe (String /\ Array (Argument arg) /\ Array (Output out))


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


instance Functor Output where
    map :: forall a b. (a -> b) -> Output a -> Output b
    map f (Output name v) = Output name $ f v


empty :: forall arg out. String -> Fn arg out
empty n = Fn $ n /\ [] /\ []


fnOf :: forall arg out. String -> Array (String /\ arg) -> Fn arg out
fnOf n args =
    Fn $ n /\ (Tuple.uncurry Argument <$> args) /\ []


singleton :: forall arg out. String -> (String /\ arg) -> Fn arg out
singleton = fn1


fn1 :: forall arg out. String -> (String /\ arg) -> Fn arg out
fn1 n a1 =
    fnOf n [ a1 ]


fn2 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn2 n a1 a2 =
    fnOf n [ a1, a2 ]


fn3 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn3 n a1 a2 a3 =
    fnOf n [ a1, a2, a3 ]


fn4 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn4 n a1 a2 a3 a4 =
    fnOf n [ a1, a2, a3, a4 ]


fn5 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn5 n a1 a2 a3 a4 a5 =
    fnOf n [ a1, a2, a3, a4, a5 ]


fn6 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn6 n a1 a2 a3 a4 a5 a6 =
    fnOf n [ a1, a2, a3, a4, a5, a6 ]


fn7 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn7 n a1 a2 a3 a4 a5 a6 a7 =
    fnOf n [ a1, a2, a3, a4, a5, a6, a7 ]


fn8 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg out
fn8 n a1 a2 a3 a4 a5 a6 a7 a8 =
    fnOf n [ a1, a2, a3, a4, a5, a6, a7, a8 ]


arg :: forall x. ArgumentName -> x -> Argument x
arg = Argument


out :: forall x. OutputName -> x -> Output x
out = Output


args :: forall arg out. Fn arg out -> Array (String /\ arg)
args (Fn (_ /\ as /\ _)) = dearg <$> as


outs :: forall arg out. Fn arg out -> Array (String /\ out)
outs (Fn (_ /\ _ /\ os)) = deout <$> os


q :: forall x. ArgumentName -> x -> Argument x
q = Argument -- TODO: private


o :: forall x. OutputName -> x -> Output x
o = Output -- TODO: private


argValue :: forall x. Argument x -> x
argValue (Argument _ x) = x


outValue :: forall x. Output x -> x
outValue (Output _ x) = x


argName :: forall x. Argument x -> ArgumentName
argName (Argument name _) = name


outputName :: forall x. Output x -> OutputName
outputName (Output name _) = name


argsCount :: forall i o. Fn i o -> Int
argsCount (Fn (_ /\ args /\ _)) = Array.length args


outsCount :: forall i o. Fn i o -> Int
outsCount (Fn (_ /\ _ /\ outs)) = Array.length outs


dearg :: forall x. Argument x -> ArgumentName /\ x
dearg (Argument name x) = name /\ x


deout :: forall x. Output x -> OutputName /\ x
deout (Output name x) = name /\ x


name :: forall i o. Fn i o -> String
name (Fn (name /\ _)) = name


toFnX :: forall a arg out. ToFn arg out a => a -> String /\ Array arg /\ Array out
toFnX a = bimap (map argValue) (map outValue) <$> (toFn a :: String /\ Array (Argument arg) /\ Array (Output out))


instance ToFn arg out (Fn arg out) where
    toFn :: Fn arg out -> String /\ Array (Argument arg) /\ Array (Output out)
    toFn (Fn fn) = fn


newtype KnownFn = KnownFn String


nameOf :: KnownFn -> String
nameOf (KnownFn name) = name
