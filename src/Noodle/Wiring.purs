module Noodle.Wiring where

import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)
import Signal.Extra (class RunInSignal)


class (MonadRec m, MonadEffect m, RunInSignal m) <= Wiring m
instance (MonadRec m, MonadEffect m, RunInSignal m) => Wiring m