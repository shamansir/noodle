module Control.Monad.Extra where

import Prelude

import Data.Maybe (Maybe, maybe)
-- import Data.Foldable (for_)


whenJust :: forall m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust mg f = maybe (pure unit) f mg