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

import Noodle.Id (Input, InputR, Output, OutputR)
import Noodle.Fn.RawToRec (toRec)
import Noodle.Fn.Raw.Tracker as Raw


type Tracker state (is :: Row Type) (os :: Row Type) repr = Raw.Tracker state repr


inputs :: forall state is os repr. Tracker state is os repr -> Effect (Map InputR repr)
inputs = Raw.inputs


outputs :: forall state is os repr. Tracker state is os repr-> Effect (Map OutputR repr)
outputs = Raw.outputs


lastInput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe InputR)
lastInput = Raw.lastInput


lastOutput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe OutputR)
lastOutput = Raw.lastOutput


inputsRec :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Tracker state is os repr -> Effect (Record is)
inputsRec tracker = Raw.inputs tracker <#> toRec


outputsRec :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Tracker state is os repr -> Effect (Record os)
outputsRec tracker = Raw.outputs tracker <#> toRec


atInput :: forall state is os repr. InputR -> Tracker state is os repr -> Effect (Maybe repr)
atInput = Raw.atInput


atOutput :: forall state is os repr. OutputR -> Tracker state is os repr -> Effect (Maybe repr)
atOutput = Raw.atOutput


-- atInputRec :: forall i is os state repr din. Row.Cons i din is is => IsSymbol i => Input i -> Tracker state is os repr -> Effect din
-- atInputRec input tracker = inputsRec tracker <#> Record.get (proxify input)


-- atOutputRec :: forall o is os osrl state repr dout. Row.Cons o dout os os => IsSymbol o => Output o -> Tracker state is os repr -> Effect dout
-- atOutputRec output tracker = outputsRec tracker <#> Record.get (proxify output)