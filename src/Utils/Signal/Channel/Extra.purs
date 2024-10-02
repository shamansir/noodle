module Signal.Channel.Extra where

import Prelude (Unit, (>>=), pure, unit)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref


import Signal.Channel (Channel)
import Signal.Channel as Ch


sendIfRef :: forall a. Channel a -> Ref Boolean -> a -> Effect Unit
sendIfRef channel ref v =
    Ref.read ref >>= \flag ->
        if flag then Ch.send channel v else pure unit