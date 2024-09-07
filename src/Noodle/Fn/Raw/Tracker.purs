module Noodle.Fn.Raw.Tracker where

import Prelude ((<#>), (>>>), ($))

import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Repr (class ToRepr, ensureTo, unwrap)

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InletR, OutletR)
import Noodle.Fn.Generic.Tracker (Tracker, inlets, outlets, lastInlet, lastOutlet, state, mapState) as Generic


type Tracker state repr = Generic.Tracker state (Map InletR repr) (Map OutletR repr)


inlets :: forall state repr. Tracker state repr -> Effect (Map InletR repr)
inlets = Generic.inlets


outlets :: forall state repr. Tracker state repr -> Effect (Map OutletR repr)
outlets = Generic.outlets


lastInlet :: forall state repr. Tracker state repr -> Effect (Maybe InletR)
lastInlet = Generic.lastInlet


lastOutlet :: forall state repr. Tracker state repr -> Effect (Maybe OutletR)
lastOutlet = Generic.lastOutlet


atInlet :: forall state repr. InletR -> Tracker state repr -> Effect (Maybe repr)
atInlet inlet tracker = inlets tracker <#> Map.lookup inlet


atOutlet :: forall state repr. OutletR -> Tracker state repr -> Effect (Maybe repr)
atOutlet outlet tracker = outlets tracker <#> Map.lookup outlet


state :: forall state repr. Tracker state repr -> Effect state
state = Generic.state


toReprableState :: forall state repr. ToRepr state repr => Tracker state repr -> Tracker repr repr
toReprableState = Generic.mapState $ ensureTo >>> unwrap