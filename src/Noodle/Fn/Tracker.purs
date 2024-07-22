module Noodle.Fn.Tracker where

import Prelude ((<#>), map)

import Data.Map (Map)
import Data.Map.Extra (stringifyKeys) as Map
import Data.SProxy (reflect')

import Effect (Effect)

import Prim.RowList as RL

import Data.Maybe (Maybe)
import Data.Repr (class FromReprRow, Repr(..))
import Data.Repr (fromMap) as Repr

import Noodle.Id (InputR, OutputR)



import Noodle.Fn.Raw.Tracker as Raw

type Tracker  state (is :: Row Type) (os :: Row Type) repr = Raw.Tracker  state repr


inputs :: forall state is os repr. Tracker state is os repr -> Effect (Map InputR repr)
inputs = Raw.inputs


outputs :: forall state is os repr. Tracker state is os repr-> Effect (Map OutputR repr)
outputs = Raw.outputs


lastInput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe InputR)
lastInput = Raw.lastInput


lastOutput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe OutputR)
lastOutput = Raw.lastOutput


inputsRec :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr => Tracker state is os repr -> Effect (Record is)
inputsRec tracker = Raw.inputs tracker <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap


outputsRec :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr => Tracker state is os repr -> Effect (Record os)
outputsRec tracker = Raw.outputs tracker <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap