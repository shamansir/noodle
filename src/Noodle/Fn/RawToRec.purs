module Noodle.Fn.RawToRec where

import Prelude (map, (>>>))

import Prim.RowList as RL

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Repr (Repr(..), class FromReprRow)
import Data.Repr (fromMap) as Repr
import Data.SProxy (class Reflect', reflect')


toRec :: forall k repr rowl row. Reflect' k => RL.RowToList row rowl => FromReprRow rowl row repr => Map k repr -> Record row
toRec = map Repr >>> Map.stringifyKeys reflect' >>> Repr.fromMap


{-
toRecOver :: forall f k repr rowl row. Functor f => Reflect' k => RL.RowToList row rowl => FromReprRow rowl row repr => f (Map k repr) -> f (Record row)
toRecOver = map toRec
-}
