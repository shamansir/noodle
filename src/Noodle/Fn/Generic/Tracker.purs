module Noodle.Fn.Generic.Tracker where

import Prelude ((<#>))

import Effect (Effect)
import Signal (Signal)
import Signal (get) as Signal

import Data.Maybe (Maybe)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\))

import Noodle.Id (InletR, OutletR)
import Noodle.Fn.Generic.Updates as U


type Tracker state inlets outlets =
    { state :: Signal state
    , inlets :: Signal (U.InletsChange /\ inlets)
    , outlets :: Signal (U.OutletsChange /\ outlets)
    , all :: Signal (U.FocusedUpdate state inlets outlets)
    }


inlets :: forall state inlets outlets. Tracker state inlets outlets -> Effect inlets
inlets tracker = Signal.get tracker.inlets <#> Tuple.snd


outlets :: forall state inlets outlets. Tracker state inlets outlets -> Effect outlets
outlets tracker = Signal.get tracker.outlets <#> Tuple.snd


lastInput :: forall state inlets outlets. Tracker state inlets outlets -> Effect (Maybe InletR)
lastInput tracker = Signal.get tracker.inlets <#> Tuple.fst <#> U.inputChangeToMaybe


lastOutput :: forall state inlets outlets. Tracker state inlets outlets -> Effect (Maybe OutletR)
lastOutput tracker = Signal.get tracker.outlets <#> Tuple.fst <#> U.outputChangeToMaybe
