module Noodle.Fn.RawToRec where

import Prelude (map, (>>>))

import Type.Proxy (Proxy)

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Repr (Repr(..), class ToReprRow, class FromReprRow)
import Data.Repr (fromMap, toMap, unwrap) as Repr
import Data.Symbol (class IsSymbol)
import Data.SProxy (class Reflect', reflect')


fromRec :: forall k repr rowl row. Reflect' k => ToReprRow rowl row k repr => (forall s. IsSymbol s => Proxy s -> k) -> Record row -> Map k repr
fromRec toKey = Repr.toMap toKey >>> map Repr.unwrap


toRec :: forall k repr rowl row. Reflect' k => FromReprRow rowl row repr => Map k repr -> Record row
toRec = map Repr >>> Map.stringifyKeys reflect' >>> Repr.fromMap


{-
toRecOver :: forall f k repr rowl row. Functor f => Reflect' k => RL.RowToList row rowl => FromReprRow rowl row repr => f (Map k repr) -> f (Record row)
toRecOver = map toRec
-}
