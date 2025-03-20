module Noodle.Repr.StRepr where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Noodle.Repr.HasFallback (class HasFallback, fallback)


-- https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html
-- import Data.Generic.Rep as GR


class StRepr a repr where
  to :: a -> repr
  from :: repr -> Maybe a


{-
instance (StRepr x repr, Newtype w x) => StRepr w repr where
  to :: w -> repr
  to = unwrap >>> to
  from :: repr -> Maybe w
  from = map wrap <<< from
-}


ensureFrom :: forall a repr. HasFallback a => StRepr a repr => repr -> a
ensureFrom = fromMaybe fallback <<< from