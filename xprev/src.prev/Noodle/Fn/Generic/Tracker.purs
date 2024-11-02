module Noodle.Fn.Generic.Tracker where

import Prelude ((<#>))

import Effect (Effect)
import Signal (Signal)
import Signal (get) as Signal

import Data.Maybe (Maybe)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\))

import Noodle.Id (InputR, OutputR)
import Noodle.Fn.Generic.Updates as U


type Tracker state inputs outputs =
    { state :: Signal state
    , inputs :: Signal (U.InputChange /\ inputs)
    , outputs :: Signal (U.OutputChange /\ outputs)
    , all :: Signal (U.FocusedUpdate state inputs outputs)
    }


inputs :: forall state inputs outputs. Tracker state inputs outputs -> Effect inputs
inputs tracker = Signal.get tracker.inputs <#> Tuple.snd


outputs :: forall state inputs outputs. Tracker state inputs outputs -> Effect outputs
outputs tracker = Signal.get tracker.outputs <#> Tuple.snd


lastInput :: forall state inputs outputs. Tracker state inputs outputs -> Effect (Maybe InputR)
lastInput tracker = Signal.get tracker.inputs <#> Tuple.fst <#> U.inputChangeToMaybe


lastOutput :: forall state inputs outputs. Tracker state inputs outputs -> Effect (Maybe OutputR)
lastOutput tracker = Signal.get tracker.outputs <#> Tuple.fst <#> U.outputChangeToMaybe
