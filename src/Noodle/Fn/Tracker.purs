module Noodle.Fn.Tracker where

import Prelude ((<#>), map)

import Record (get) as Record

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.Symbol (class IsSymbol)

import Effect (Effect)

import Prim.Row as Row
import Prim.RowList as RL

import Data.Maybe (Maybe)
import Data.Repr (class FromReprRow, Repr(..))
import Data.Repr (fromMap) as Repr

import Noodle.Id (Inlet, InletR, Outlet, OutletR)
import Noodle.Id (inletRName, outletRName) as Id
import Noodle.Raw.FromToRec (toRec)
import Noodle.Raw.Fn.Tracker as Raw


type Tracker state (is :: Row Type) (os :: Row Type) repr = Raw.Tracker state repr


inlets :: forall state is os repr. Tracker state is os repr -> Effect (Map InletR repr)
inlets = Raw.inlets


outlets :: forall state is os repr. Tracker state is os repr-> Effect (Map OutletR repr)
outlets = Raw.outlets


lastInlet :: forall state is os repr. Tracker state is os repr -> Effect (Maybe InletR)
lastInlet = Raw.lastInlet


lastOutlet :: forall state is os repr. Tracker state is os repr -> Effect (Maybe OutletR)
lastOutlet = Raw.lastOutlet


inletsRec :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Tracker state is os repr -> Effect (Record is)
inletsRec tracker = Raw.inlets tracker <#> toRec Id.inletRName


outletsRec :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Tracker state is os repr -> Effect (Record os)
outletsRec tracker = Raw.outlets tracker <#> toRec Id.outletRName


atInlet :: forall state is os repr. InletR -> Tracker state is os repr -> Effect (Maybe repr)
atInlet = Raw.atInlet


atOutlet :: forall state is os repr. OutletR -> Tracker state is os repr -> Effect (Maybe repr)
atOutlet = Raw.atOutlet


-- atInletRec :: forall i is os state repr din. Row.Cons i din is is => IsSymbol i => Inlet i -> Tracker state is os repr -> Effect din
-- atInletRec inlet tracker = inletsRec tracker <#> Record.get (proxify inlet)


-- atOutletRec :: forall o is os osrl state repr dout. Row.Cons o dout os os => IsSymbol o => Outlet o -> Tracker state is os repr -> Effect dout
-- atOutletRec outlet tracker = outletsRec tracker <#> Record.get (proxify outlet)