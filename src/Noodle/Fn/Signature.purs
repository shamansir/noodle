module Noodle.Fn.Signature where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (fst, snd, uncurry) as Tuple
import Data.Maybe (Maybe(..), maybe)
import Data.Array ((:))
import Data.Array (length, sortWith) as Array
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (joinWith) as String
import Data.String.Extra (pascalCase) as String
import Data.Bifunctor (class Bifunctor)
import Data.Functor.Extra ((<$$>))

import Type.Proxy (Proxy(..))

import Data.Bifunctor (bimap)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel)
import Noodle.Repr.ValueInChannel (fromValueInChannel, accept, empty) as ViC

import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.Code.Target (JS, PS, pureScript, javaScript)

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


type SignatureS arg out = String /\ Array (Argument arg) /\ Array (Output out)


type SignatureX arg out = String /\ Array arg /\ Array out


newtype Signature arg out = Sig (SignatureS arg out)


instance Bifunctor Signature where
    bimap f g (Sig fnS) = Sig $ bimap (map $ map f) (map $ map g) <$> fnS


derive instance Newtype (Signature arg out) _


type SignatureU arg = Signature arg Unit



derive instance (Eq arg) => Eq (Argument arg)
derive instance (Eq out) => Eq (Output out)
derive newtype instance (Eq arg, Eq out) => Eq (Signature arg out)


class ToSignature :: forall k. k -> Type -> Type -> Type -> Constraint -- TODO: Leave only `PossiblyToSignature` and rename it to `ToSignature` with always returning `Maybe`
class ToSignature x arg out a where
    toSignature :: Proxy x -> a -> Signature arg out


class PossiblyToSignature :: forall k. k -> Type -> Type -> Type -> Constraint
class PossiblyToSignature x arg out a where
    possiblyToSignature :: Proxy x -> a -> Maybe (Signature arg out)


{-
instance (ToSignature x arg out a) => PossiblyToSignature x arg out a where
    possiblyToSignature pa = Just <<< toSignature pa
-}


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


instance Functor Output where
    map :: forall a b. (a -> b) -> Output a -> Output b
    map f (Output name v) = Output name $ f v


empty :: forall arg out. String -> Signature arg out
empty n = Sig $ n /\ [] /\ []


sigs :: forall arg out. SignatureS arg out -> Signature arg out
sigs = wrap


sig :: forall arg out. String -> Array (Argument arg) -> Array (Output out) -> Signature arg out
sig n args outs =
    Sig $ n /\ args /\ outs


sig' :: forall arg out. String -> Array (String /\ arg) -> Array (String /\ out) -> Signature arg out
sig' n args outs =
    sig n (Tuple.uncurry Argument <$> args) (Tuple.uncurry Output <$> outs)


sigOf :: forall arg out. String -> Array (String /\ arg) -> Signature arg out
sigOf n args =
    Sig $ n /\ (Tuple.uncurry Argument <$> args) /\ []


singleton :: forall arg out. String -> (String /\ arg) -> Signature arg out
singleton = sig1


sig1 :: forall arg out. String -> (String /\ arg) -> Signature arg out
sig1 n a1 =
    sigOf n [ a1 ]


sig2 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig2 n a1 a2 =
    sigOf n [ a1, a2 ]


sig3 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig3 n a1 a2 a3 =
    sigOf n [ a1, a2, a3 ]


sig4 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig4 n a1 a2 a3 a4 =
    sigOf n [ a1, a2, a3, a4 ]


sig5 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig5 n a1 a2 a3 a4 a5 =
    sigOf n [ a1, a2, a3, a4, a5 ]


sig6 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig6 n a1 a2 a3 a4 a5 a6 =
    sigOf n [ a1, a2, a3, a4, a5, a6 ]


sig7 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig7 n a1 a2 a3 a4 a5 a6 a7 =
    sigOf n [ a1, a2, a3, a4, a5, a6, a7 ]


sig8 :: forall arg out. String -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> (String /\ arg) -> Signature arg out
sig8 n a1 a2 a3 a4 a5 a6 a7 a8 =
    sigOf n [ a1, a2, a3, a4, a5, a6, a7, a8 ]


addOuts :: forall arg out. Array (String /\ out) -> Signature arg out -> Signature arg out
addOuts next_outs (Sig (name /\ args /\ cur_outs)) =
    Sig $ name /\ args /\ (cur_outs <> (Tuple.uncurry Output <$> next_outs))


out1 :: forall arg out. (String /\ out) -> Signature arg out -> Signature arg out
out1 o1 =
    addOuts [ o1 ]


out2 :: forall arg out. (String /\ out) -> (String /\ out) -> Signature arg out -> Signature arg out
out2 o1 o2 =
    addOuts [ o1, o2 ]


out3 :: forall arg out. (String /\ out) -> (String /\ out) -> (String /\ out) -> Signature arg out -> Signature arg out
out3 o1 o2 o3 =
    addOuts [ o1, o2, o3 ]


arg :: forall x. ArgumentName -> x -> Argument x
arg = i


out :: forall x. OutputName -> x -> Output x
out = o


args :: forall arg out. Signature arg out -> Array (String /\ arg)
args (Sig (_ /\ as /\ _)) = dearg <$> as


outs :: forall arg out. Signature arg out -> Array (String /\ out)
outs (Sig (_ /\ _ /\ os)) = deout <$> os


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


argsCount :: forall i o. Signature i o -> Int
argsCount (Sig (_ /\ args /\ _)) = Array.length args


outsCount :: forall i o. Signature i o -> Int
outsCount (Sig (_ /\ _ /\ outs)) = Array.length outs


dearg :: forall x. Argument x -> ArgumentName /\ x
dearg (Argument name x) = name /\ x


deout :: forall x. Output x -> OutputName /\ x
deout (Output name x) = name /\ x


name :: forall i o. Signature i o -> String
name (Sig (name /\ _)) = name


extract :: forall x a arg out. ToSignature x arg out a => Proxy x -> a -> SignatureX arg out
extract px a = bimap (map argValue) (map outValue) <$> unwrap (toSignature px a :: Signature arg out)


toReprable :: forall x arg out a repr. FromValueInChannel arg repr => FromValueInChannel out repr => ToSignature x arg out a => Proxy x -> a -> Signature repr repr
toReprable px a = bimap ViC.fromValueInChannel ViC.fromValueInChannel (toSignature px a :: Signature arg out)


toChanneled :: forall arg out. Signature (Maybe arg) (Maybe out) -> Signature (ValueInChannel arg) (ValueInChannel out)
toChanneled = bimap (maybe ViC.empty ViC.accept) (maybe ViC.empty ViC.accept)


reorder :: forall a b arg out. Ord a => Ord b => (ArgumentName -> a) -> (OutputName -> b) -> Signature arg out -> Signature arg out
reorder farg fout = _reorder (argName >>> farg) (outName >>> fout)


_reorder :: forall a b arg out. Ord a => Ord b => (Argument arg -> a) -> (Output out -> b) -> Signature arg out -> Signature arg out
_reorder farg fout (Sig (name /\ args /\ outs)) = Sig (name /\ Array.sortWith farg args /\ Array.sortWith fout outs)


instance ToSignature Void arg out (Signature arg out) where
    toSignature :: Proxy Void -> Signature arg out -> Signature arg out
    toSignature = const identity


instance (Show arg, Show out) => Show (Signature arg out) where
    show = defaultShow


_showManually :: forall arg out. (arg -> String) -> (out -> String) -> Signature arg out -> String
_showManually showArg showOut = unwrap >>> case _ of
        name /\ args /\ outs ->
            if Array.length args > 0 && Array.length outs > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (argValue <$> showArg <$$> args) <> " -> " <> String.joinWith " " (outValue <$> showOut <$$> outs) <> ">"
            else if Array.length args > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (argValue <$> showArg <$$> args) <> ">"
            else if Array.length outs > 0 then
                "<" <> String.pascalCase name <> " -> " <> String.joinWith " " (outValue <$> showOut <$$> outs) <> ">"
            else
                 "<" <> String.pascalCase name <> ">"


defaultShow :: forall arg out. Show arg => Show out => Signature arg out -> String
defaultShow = _showManually show show


showUsingSignature :: forall x arg out a. Show arg => Show out => ToSignature x arg out a => Proxy x -> a -> String
showUsingSignature px a = defaultShow (toSignature px a :: Signature arg out)


instance ToCode JS opts arg => ToCode JS opts (Signature arg out) where
    toCode :: Proxy JS -> opts -> Signature arg out -> String
    toCode _ opts sig = case args sig of
        [] -> name sig <> "()"
        arguments -> fnsJs (name sig) $ toCode javaScript opts <$> Tuple.snd <$> arguments
        where
            fnsJs :: String -> Array String -> String
            fnsJs name vals = name <> "( " <> (String.joinWith ", " vals) <> " )"


instance ToCode PS opts arg => ToCode PS opts (Signature arg out) where
    toCode :: Proxy PS -> opts -> Signature arg out -> String
    toCode _ opts sig = case args sig of
        [] -> name sig
        arguments -> fnsPs (name sig) $ toCode pureScript opts <$> Tuple.snd <$> arguments
        where
            fnsPs :: String -> Array String -> String
            fnsPs name vals = name <> " " <> (String.joinWith " " vals)


toPureScript :: forall opts arg out. ToCode PS opts arg => opts -> Signature arg out -> String
toPureScript = toCode pureScript


toPureScript' :: forall arg out. ToCode PS Unit arg => Signature arg out -> String
toPureScript' = toPureScript unit


toJavaScript :: forall opts arg out. ToCode PS opts arg => opts -> Signature arg out -> String
toJavaScript = toCode pureScript


toJavaScript' :: forall arg out. ToCode PS Unit arg => Signature arg out -> String
toJavaScript' = toJavaScript unit