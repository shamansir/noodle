module Tookit.Hydra.Lang.Fn where


import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (snd, uncurry) as Tuple
import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (length) as Array


-- import Tookit.Hydra.Types
-- import Tookit.Hydra.Repr.Wrap (WrapRepr)



-- TODO: use typelevel arguments counts like in https://pursuit.purescript.org/packages/purescript-fast-vect/1.1.0
--       or in example/Hydra/Fn


data Argument x = Argument String x


instance Show x => Show (Argument x) where
    show (Argument name v) = name <> ":" <> show v


type ArgumentName = String


-- TODO: use in `ToFn` & PossinlyToFn
-- TODO: add output type
newtype Fn arg = Fn (String /\ Array (Argument arg))


type FnS arg = String /\ Array (Argument arg)


class ToFn arg a where
    toFn :: a -> String /\ Array (Argument arg)


class PossiblyToFn arg a where
    possiblyToFn :: a -> Maybe (String /\ Array (Argument arg))


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


empty :: forall arg. String -> Fn arg
empty n = Fn $ n /\ []


fnOf :: forall arg. String -> Array (String /\ arg) -> Fn arg
fnOf n args =
    Fn $ n /\ (Tuple.uncurry Argument <$> args)


singleton :: forall arg. String -> (String /\ arg) -> Fn arg
singleton = fn1


fn1 :: forall arg. String -> (String /\ arg) -> Fn arg
fn1 n a1 =
    fnOf n [ a1 ]


fn2 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn2 n a1 a2 =
    fnOf n [ a1, a2 ]


fn3 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn3 n a1 a2 a3 =
    fnOf n [ a1, a2, a3 ]


fn4 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn4 n a1 a2 a3 a4 =
    fnOf n [ a1, a2, a3, a4 ]


fn5 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn5 n a1 a2 a3 a4 a5 =
    fnOf n [ a1, a2, a3, a4, a5 ]


fn6 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn6 n a1 a2 a3 a4 a5 a6 =
    fnOf n [ a1, a2, a3, a4, a5, a6 ]


fn7 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn7 n a1 a2 a3 a4 a5 a6 a7 =
    fnOf n [ a1, a2, a3, a4, a5, a6, a7 ]


fn8 :: forall arg. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Fn arg
fn8 n a1 a2 a3 a4 a5 a6 a7 a8 =
    fnOf n [ a1, a2, a3, a4, a5, a6, a7, a8 ]


arg :: forall x. ArgumentName -> x -> Argument x
arg = Argument


args :: forall x. Fn x -> Array (String /\ x)
args (Fn (_ /\ as)) = dearg <$> as


q :: forall x. ArgumentName -> x -> Argument x
q = Argument -- TODO: private


argValue :: forall x. Argument x -> x
argValue (Argument _ x) = x


argName :: forall x. Argument x -> ArgumentName
argName (Argument name _) = name


argsCount :: forall x. Fn x -> Int
argsCount (Fn (_ /\ args)) = Array.length args


dearg :: forall x. Argument x -> ArgumentName /\ x
dearg (Argument name x) = name /\ x


name :: forall x. Fn x -> String
name (Fn (name /\ _)) = name


toFnX :: forall a arg. ToFn arg a => a -> String /\ Array arg
toFnX a = map argValue <$> (toFn a :: String /\ Array (Argument arg))


instance ToFn arg (Fn arg) where
    toFn :: Fn arg -> String /\ Array (Argument arg)
    toFn (Fn fn) = fn


newtype KnownFn = KnownFn String


nameOf :: KnownFn -> String
nameOf (KnownFn name) = name
