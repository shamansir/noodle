module Noodle.Fn.Tracker where

import Prelude ((<#>))

import Data.Map (Map)

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InletR, OutletR)
import Noodle.Id (inletRName, outletRName) as Id
import Noodle.Raw.FromToRec (toRec)
import Noodle.Raw.Fn.Tracker as Raw
import Noodle.Repr.ValueInChannel (ValueInChannel, class ToValuesInChannelRow)
import Noodle.Repr.StRepr (class StRepr)


type Tracker state (is :: Row Type) (os :: Row Type) chrepr = Raw.Tracker state chrepr


inlets :: forall state is os chrepr. Tracker state is os chrepr -> Effect (Map InletR (ValueInChannel chrepr))
inlets = Raw.inlets


outlets :: forall state is os chrepr. Tracker state is os chrepr -> Effect (Map OutletR (ValueInChannel chrepr))
outlets = Raw.outlets


lastInlet :: forall state is os chrepr. Tracker state is os chrepr -> Effect (Maybe InletR)
lastInlet = Raw.lastInlet


lastOutlet :: forall state is os chrepr. Tracker state is os chrepr -> Effect (Maybe OutletR)
lastOutlet = Raw.lastOutlet


inletsRec :: forall state is isrl os chrepr. ToValuesInChannelRow isrl is chrepr => Tracker state is os chrepr -> Effect (Record is)
inletsRec tracker = Raw.inlets tracker <#> toRec Id.inletRName


outletsRec :: forall state is os osrl chrepr. ToValuesInChannelRow osrl os chrepr => Tracker state is os chrepr -> Effect (Record os)
outletsRec tracker = Raw.outlets tracker <#> toRec Id.outletRName


atInlet :: forall state is os chrepr. InletR -> Tracker state is os chrepr -> Effect (Maybe (ValueInChannel chrepr))
atInlet = Raw.atInlet


atOutlet :: forall state is os chrepr. OutletR -> Tracker state is os chrepr -> Effect (Maybe (ValueInChannel chrepr))
atOutlet = Raw.atOutlet


toReprableState :: forall state strepr is os chrepr. StRepr state strepr => Tracker state is os chrepr -> Tracker strepr is os chrepr
toReprableState = Raw.toReprableState


-- atInletRec :: forall i is os state repr din. Row.Cons i din is is => IsSymbol i => Inlet i -> Tracker state is os repr -> Effect din
-- atInletRec inlet tracker = inletsRec tracker <#> Record.get (proxify inlet)


-- atOutletRec :: forall o is os osrl state repr dout. Row.Cons o dout os os => IsSymbol o => Outlet o -> Tracker state is os repr -> Effect dout
-- atOutletRec outlet tracker = outletsRec tracker <#> Record.get (proxify outlet)