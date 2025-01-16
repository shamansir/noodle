module Noodle.Raw.FromToRec where

import Prelude (map, (>>>))

import Type.Proxy (Proxy)

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Symbol (class IsSymbol)

import Noodle.Repr.ValueInChannel (ValueInChannel, class ToValuesInChannelRow, class FromValuesInChannelRow)
import Noodle.Repr.ValueInChannel (fromMap, toMap) as VsiC


fromRec :: forall k repr rowl row. FromValuesInChannelRow rowl row k repr => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k (ValueInChannel repr)
fromRec toKey = VsiC.toMap toKey


toRec :: forall k repr rowl row. ToValuesInChannelRow rowl row repr => (k -> String) -> Map k (ValueInChannel repr) -> Record row
toRec toString = Map.stringifyKeys toString >>> VsiC.fromMap
