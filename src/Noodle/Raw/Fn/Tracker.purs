module Noodle.Raw.Fn.Tracker where

import Prelude ((<#>), (>>>), ($))

import Data.Map (Map)
import Data.Map (lookup) as Map

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InletR, OutletR)
import Noodle.Fn.Generic.Tracker (Tracker, inlets, outlets, lastInlet, lastOutlet, state, mapState) as Generic
import Noodle.Repr.ChRepr (ValueInChannel)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (to) as StRepr


type Tracker state chrepr = Generic.Tracker state (Map InletR (ValueInChannel chrepr)) (Map OutletR (ValueInChannel chrepr))


inlets :: forall state chrepr. Tracker state chrepr -> Effect (Map InletR (ValueInChannel chrepr))
inlets = Generic.inlets


outlets :: forall state chrepr. Tracker state chrepr -> Effect (Map OutletR (ValueInChannel chrepr))
outlets = Generic.outlets


lastInlet :: forall state chrepr. Tracker state chrepr -> Effect (Maybe InletR)
lastInlet = Generic.lastInlet


lastOutlet :: forall state chrepr. Tracker state chrepr -> Effect (Maybe OutletR)
lastOutlet = Generic.lastOutlet


atInlet :: forall state chrepr. InletR -> Tracker state chrepr -> Effect (Maybe (ValueInChannel chrepr))
atInlet inlet tracker = inlets tracker <#> Map.lookup inlet


atOutlet :: forall state chrepr. OutletR -> Tracker state chrepr -> Effect (Maybe (ValueInChannel chrepr))
atOutlet outlet tracker = outlets tracker <#> Map.lookup outlet


state :: forall state chrepr. Tracker state chrepr -> Effect state
state = Generic.state


toReprableState :: forall state strepr chrepr. StRepr state strepr => Tracker state chrepr -> Tracker strepr chrepr
toReprableState = Generic.mapState StRepr.to