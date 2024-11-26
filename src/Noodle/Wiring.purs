module Noodle.Wiring where

import Effect.Class (class MonadEffect)
import Effect.Exception (Error) as Ex
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Error.Class (class MonadThrow)
import Signal.Extra (class RunInSignal)


class (MonadRec m, MonadEffect m, RunInSignal m, MonadThrow Ex.Error m) <= Wiring m
instance (MonadRec m, MonadEffect m, RunInSignal m, MonadThrow Ex.Error m) => Wiring m