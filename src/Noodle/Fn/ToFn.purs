module Noodle.Fn.ToFn where -- TODO: ensure it is a right location

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (snd, uncurry) as Tuple
import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (length, sortWith) as Array
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (joinWith) as String
import Data.String.Extra (pascalCase) as String
import Data.Bifunctor (class Bifunctor)

import Type.Proxy (Proxy(..))

import Data.Bifunctor (bimap)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class FromValueInChannel)
import Noodle.Repr.ChRepr (fromValueInChannel) as ViC

-- import Toolkit.Hydra.Types
-- import Toolkit.Hydra.Repr.Wrap (WrapRepr)



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


type FnS arg out = String /\ Array (Argument arg) /\ Array (Output out)


type FnX arg out = String /\ Array arg /\ Array out


newtype Fn arg out = Fn (FnS arg out)


instance Bifunctor Fn where
    bimap f g (Fn fnS) = Fn $ bimap (map $ map f) (map $ map g) <$> fnS


derive instance Newtype (Fn arg out) _


type FnU arg = Fn arg Unit



derive instance (Eq arg) => Eq (Argument arg)
derive instance (Eq out) => Eq (Output out)
derive newtype instance (Eq arg, Eq out) => Eq (Fn arg out)


class ToFn :: forall k. k -> Type -> Type -> Type -> Constraint
class ToFn x arg out a where
    toFn :: Proxy x -> a -> Fn arg out


class PossiblyToFn :: forall k. k -> Type -> Type -> Type -> Constraint
class PossiblyToFn x arg out a where
    possiblyToFn :: Proxy x -> a -> Maybe (Fn arg out)


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


instance Functor Output where
    map :: forall a b. (a -> b) -> Output a -> Output b
    map f (Output name v) = Output name $ f v


empty :: forall arg out. String -> Fn arg out
empty n = Fn $ n /\ [] /\ []


fns :: forall arg out. FnS arg out -> Fn arg out
fns = wrap


fn :: forall arg out. String -> Array (Argument arg) -> Array (Output out) -> Fn arg out
fn n args outs =
    Fn $ n /\ args /\ outs


fn' :: forall arg out. String -> Array (String /\ arg) -> Array (String /\ out) -> Fn arg out
fn' n args outs =
    fn n (Tuple.uncurry Argument <$> args) (Tuple.uncurry Output <$> outs)


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


addOuts :: forall arg out. Array (String /\ out) -> Fn arg out -> Fn arg out
addOuts next_outs (Fn (name /\ args /\ cur_outs)) =
    Fn $ name /\ args /\ (cur_outs <> (Tuple.uncurry Output <$> next_outs))


out1 :: forall arg out. (String /\ out) -> Fn arg out -> Fn arg out
out1 o1 =
    addOuts [ o1 ]


out2 :: forall arg out. (String /\ out) -> (String /\ out) -> Fn arg out -> Fn arg out
out2 o1 o2 =
    addOuts [ o1, o2 ]


out3 :: forall arg out. (String /\ out) -> (String /\ out) -> (String /\ out) -> Fn arg out -> Fn arg out
out3 o1 o2 o3 =
    addOuts [ o1, o2, o3 ]


arg :: forall x. ArgumentName -> x -> Argument x
arg = i


out :: forall x. OutputName -> x -> Output x
out = o


args :: forall arg out. Fn arg out -> Array (String /\ arg)
args (Fn (_ /\ as /\ _)) = dearg <$> as


outs :: forall arg out. Fn arg out -> Array (String /\ out)
outs (Fn (_ /\ _ /\ os)) = deout <$> os


i :: forall x. ArgumentName -> x -> Argument x
i = Argument -- TODO: private


in_ :: forall x. ArgumentName -> x -> Argument (Maybe x)
in_ n = i n <<< Just


inx_ :: forall x. ArgumentName -> Argument (Maybe x)
inx_ n = i n $ Nothing


o :: forall x. OutputName -> x -> Output x
o = Output -- TODO: private


out_ :: forall x. OutputName -> x -> Output (Maybe x)
out_ n = o n <<< Just


outx_ :: forall x. OutputName -> Output (Maybe x)
outx_ n = o n $ Nothing


argValue :: forall x. Argument x -> x
argValue (Argument _ x) = x


outValue :: forall x. Output x -> x
outValue (Output _ x) = x


argName :: forall x. Argument x -> ArgumentName
argName (Argument name _) = name


outName :: forall x. Output x -> OutputName
outName (Output name _) = name


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


extract :: forall x a arg out. ToFn x arg out a => Proxy x -> a -> FnX arg out
extract px a = bimap (map argValue) (map outValue) <$> unwrap (toFn px a :: Fn arg out)


toReprable :: forall x arg out a repr. FromValueInChannel arg repr => FromValueInChannel out repr => ToFn x arg out a => Proxy x -> a -> Fn repr repr
toReprable px a = bimap ViC.fromValueInChannel ViC.fromValueInChannel (toFn px a :: Fn arg out)


reorder :: forall a b arg out. Ord a => Ord b => (ArgumentName -> a) -> (OutputName -> b) -> Fn arg out -> Fn arg out
reorder farg fout = _reorder (argName >>> farg) (outName >>> fout)


_reorder :: forall a b arg out. Ord a => Ord b => (Argument arg -> a) -> (Output out -> b) -> Fn arg out -> Fn arg out
_reorder farg fout (Fn (name /\ args /\ outs)) = Fn (name /\ Array.sortWith farg args /\ Array.sortWith fout outs)


instance ToFn Void arg out (Fn arg out) where
    toFn :: Proxy Void -> Fn arg out -> Fn arg out
    toFn = const identity


instance (Show arg, Show out) => Show (Fn arg out) where
    show = defaultShow


_showManually :: forall arg out. (arg -> String) -> (out -> String) -> Fn arg out -> String
_showManually showArg showOut = unwrap >>> case _ of
        name /\ args /\ outs ->
            if Array.length args > 0 && Array.length outs > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> map showArg <$> args) <> " -> " <> String.joinWith " " (show <$> map showOut <$> outs) <> ">"
            else if Array.length args > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> map showArg <$> args) <> ">"
            else if Array.length outs > 0 then
                "<" <> String.pascalCase name <> " -> " <> String.joinWith " " (show <$> map showOut <$> outs) <> ">"
            else
                 "<" <> String.pascalCase name <> ">"

defaultShow :: forall arg out. Show arg => Show out => Fn arg out -> String
defaultShow = _showManually show show


showUsingFn :: forall x arg out a. Show arg => Show out => ToFn x arg out a => Proxy x -> a -> String
showUsingFn px a = defaultShow (toFn px a :: Fn arg out)