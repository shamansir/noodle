module Noodle.Fn.RawToRec where

import Prelude (map, (>>>))

import Type.Proxy (Proxy)

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Repr (Repr(..), class ToReprRow, class FromReprRow)
import Data.Repr (fromMap, toMap, unwrap) as Repr
import Data.Symbol (class IsSymbol)


fromRec :: forall k repr rowl row. ToReprRow rowl row k repr => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k repr
fromRec toKey = Repr.toMap toKey >>> map Repr.unwrap


toRec :: forall k repr rowl row. FromReprRow rowl row repr => (k -> String) -> Map k repr -> Record row
toRec toString = map Repr >>> Map.stringifyKeys toString >>> Repr.fromMap
