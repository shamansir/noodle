module Noodle.Repr.HasFallback where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))


class HasFallback a where
    fallback :: a


instance HasFallback Unit    where fallback = unit
instance HasFallback Int     where fallback = 0
instance HasFallback Number  where fallback = 0.0
instance HasFallback Boolean where fallback = false
instance HasFallback String  where fallback = ""
instance (HasFallback a, HasFallback b) => HasFallback (Tuple a b) where fallback = Tuple fallback fallback


fallbackBy:: forall a b. HasFallback a => (a -> b) -> Maybe a -> b
fallbackBy f = maybe (f fallback) f