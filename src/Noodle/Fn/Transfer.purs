module Noodle.Fn.Transfer where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))


newtype Receive i d = Receive { last :: Maybe i, fromInputs :: i /-> d }


instance functorReceive :: Functor (Receive i) where
    map f (Receive { last, fromInputs }) = Receive { last, fromInputs : f <$> fromInputs }


newtype Pass o d = Pass { toOutputs :: o /-> d }


instance functorPass :: Functor (Pass o) where
    map f (Pass { toOutputs }) = Pass { toOutputs : f <$> toOutputs }


newtype Send o d = Send (o -> d -> Effect Unit)


newtype Send' i d = Send' (i -> d -> Effect Unit)


r :: forall i d. Ord i => Array (i /\ d) -> Receive i d
r arr = Receive { last : Nothing, fromInputs : Map.fromFoldable arr }


r' :: forall i d. Ord i => i -> Array (i /\ d) -> Receive i d
r' last arr = Receive { last : Just last, fromInputs : Map.fromFoldable arr }


s :: forall o d. (o -> d -> Effect Unit) -> Send o d
s = Send
