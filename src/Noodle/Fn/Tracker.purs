module Noodle.Fn.Tracker where

import Prelude ((<#>), map)

import Record (get) as Record

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.SProxy (proxify, reflect')
import Data.Symbol (class IsSymbol)

import Effect (Effect)

import Prim.Row as Row
import Prim.RowList as RL

import Data.Maybe (Maybe)
import Data.Repr (class FromReprRow, Repr(..))
import Data.Repr (fromMap) as Repr

import Noodle.Id (Input, InletR, Output, OutletR)
import Noodle.Fn.RawToRec (toRec)
import Noodle.Fn.Raw.Tracker as Raw


type Tracker state (is :: Row Type) (os :: Row Type) repr = Raw.Tracker state repr


inlets :: forall state is os repr. Tracker state is os repr -> Effect (Map InletR repr)
inlets = Raw.inlets


outlets :: forall state is os repr. Tracker state is os repr-> Effect (Map OutletR repr)
outlets = Raw.outlets


lastInput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe InletR)
lastInput = Raw.lastInput


lastOutput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe OutletR)
lastOutput = Raw.lastOutput


inletsRec :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Tracker state is os repr -> Effect (Record is)
inletsRec tracker = Raw.inlets tracker <#> toRec


outletsRec :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Tracker state is os repr -> Effect (Record os)
outletsRec tracker = Raw.outlets tracker <#> toRec


atInput :: forall state is os repr. InletR -> Tracker state is os repr -> Effect (Maybe repr)
atInput = Raw.atInput


atOutput :: forall state is os repr. OutletR -> Tracker state is os repr -> Effect (Maybe repr)
atOutput = Raw.atOutput


-- atInletRec :: forall i is os state repr din. Row.Cons i din is is => IsSymbol i => Input i -> Tracker state is os repr -> Effect din
-- atInletRec input tracker = inletsRec tracker <#> Record.get (proxify input)


-- atOutletRec :: forall o is os osrl state repr dout. Row.Cons o dout os os => IsSymbol o => Output o -> Tracker state is os repr -> Effect dout
-- atOutletRec output tracker = outletsRec tracker <#> Record.get (proxify output)