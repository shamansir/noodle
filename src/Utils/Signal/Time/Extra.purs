module Signal.Time.Extra where

import Prelude
import Effect (Effect)
import Signal (Signal, constant)
import Signal.Time (Time, now)


foreign import everyP :: forall c. Effect Time -> (c -> Signal c) -> Time -> { signal :: Signal Time, cancel :: Effect Unit }


every :: Time -> { signal :: Signal Time, cancel :: Effect Unit }
every = everyP now constant