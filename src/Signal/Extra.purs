module Signal.Extra where

import Prelude

import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

import Signal (Signal)
import Signal (runSignal) as Signal


runSignal :: forall m. MonadEffect m => Signal (m Unit) -> m Unit
runSignal = unsafeCoerce $ Signal.runSignal