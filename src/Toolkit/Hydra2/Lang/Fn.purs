module Toolkit.Hydra2.Lang.Fn where


import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (snd) as Tuple
import Data.Maybe (Maybe(..))
import Data.Array ((:))


-- import Toolkit.Hydra2.Types
-- import Toolkit.Hydra2.Repr.Wrap (WrapRepr)



-- TODO: use typelevel arguments counts like in https://pursuit.purescript.org/packages/purescript-fast-vect/1.1.0
--       or in example/Hydra/Fn


data Argument x = Argument String x


instance Show x => Show (Argument x) where
    show (Argument name v) = name <> ":" <> show v


type ArgumentName = String


-- TODO
newtype Fn arg = Fn (String /\ Array (Argument arg))


class ToFn arg a where
    toFn :: a -> String /\ Array (Argument arg)


class PossiblyToFn arg a where
    possiblyToFn :: a -> Maybe (String /\ Array (Argument arg))


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


empty :: forall arg. String -> Fn arg
empty n = Fn $ n /\ []


arg :: forall x. ArgumentName -> x -> Argument x
arg = Argument


q :: forall x. ArgumentName -> x -> Argument x
q = Argument -- TODO: private


argValue :: forall x. Argument x -> x
argValue (Argument _ x) = x


argName :: forall x. Argument x -> ArgumentName
argName (Argument name _) = name


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
