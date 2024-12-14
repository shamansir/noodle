module Noodle.Raw.FromToRec where

import Prelude (map, (>>>))

import Type.Proxy (Proxy)

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Symbol (class IsSymbol)

import Noodle.Repr.ChRepr (ChRepr(..), class ToChReprRow, class FromChReprRow)
import Noodle.Repr.ChRepr (fromMap, toMap, unwrap) as ChRepr


fromRec :: forall k repr rowl row. ToChReprRow rowl row k repr => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k repr
fromRec toKey = ChRepr.toMap toKey >>> map ChRepr.unwrap


toRec :: forall k repr rowl row. FromChReprRow rowl row repr => (k -> String) -> Map k repr -> Record row
toRec toString = map ChRepr >>> Map.stringifyKeys toString >>> ChRepr.fromMap
