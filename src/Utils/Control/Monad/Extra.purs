module Control.Monad.Extra where

import Prelude

import Data.Maybe (Maybe, maybe)
-- import Data.Foldable (for_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))


whenJust :: forall m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust mg f = maybe (pure unit) f mg


whenJust2 :: forall m a b. Applicative m => Maybe a -> Maybe b -> (a -> b -> m Unit) -> m Unit
whenJust2 ma mb = whenJust ((/\) <$> ma <*> mb) <<< uncurry


whenJust_ :: forall m a. Applicative m => (a -> m Unit) -> Maybe a -> m Unit
whenJust_ = flip whenJust