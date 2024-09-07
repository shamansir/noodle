module Noodle.Fn.Generic.Tracker where

import Prelude ((<#>), (<$>), map)

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
    , inlets :: Signal (U.InletsUpdate /\ inlets)
    , outlets :: Signal (U.OutletsUpdate /\ outlets)
    , all :: Signal (U.MergedUpdate state inlets outlets)
    }


inlets :: forall state inlets outlets. Tracker state inlets outlets -> Effect inlets
inlets tracker = Signal.get tracker.inlets <#> Tuple.snd


outlets :: forall state inlets outlets. Tracker state inlets outlets -> Effect outlets
outlets tracker = Signal.get tracker.outlets <#> Tuple.snd


lastInlet :: forall state inlets outlets. Tracker state inlets outlets -> Effect (Maybe InletR)
lastInlet tracker = Signal.get tracker.inlets <#> Tuple.fst <#> U.inletUpdateToMaybe


lastOutlet :: forall state inlets outlets. Tracker state inlets outlets -> Effect (Maybe OutletR)
lastOutlet tracker = Signal.get tracker.outlets <#> Tuple.fst <#> U.outletUpdateToMaybe


state :: forall state inlets outlets. Tracker state inlets outlets -> Effect state
state tracker = Signal.get tracker.state


mapState :: forall state state' inlets outlets. (state -> state') -> Tracker state inlets outlets -> Tracker state' inlets outlets
mapState f { state, inlets, outlets, all } =
    { state : f <$> state
    , inlets
    , outlets
    , all : U.mergedMapState f <$> all
    }


mapInlets :: forall state inlets inlets' outlets. (inlets -> inlets') -> Tracker state inlets outlets -> Tracker state inlets' outlets
mapInlets f { state, inlets, outlets, all } =
    { state
    , inlets : map f <$> inlets
    , outlets
    , all : U.mergedMapInlets f <$> all
    }


mapOutlets :: forall state inlets outlets outlets'. (outlets -> outlets') -> Tracker state inlets outlets -> Tracker state inlets outlets'
mapOutlets f { state, inlets, outlets, all } =
    { state
    , inlets
    , outlets : map f <$> outlets
    , all : U.mergedMapOutlets f <$> all
    }
