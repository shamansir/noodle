module Noodle.Fn.Protocol
  ( Protocol
  , Tracker
  , make
  , inputs, outputs
  , lastInput, lastOutput
  , inputsRec, outputsRec
  )
  where

import Prelude

import Prim.RowList as RL

import Data.Map as Map
import Data.Map (Map)
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.SProxy (reflect, reflect')
import Data.Repr (class FromReprRow, Repr(..))
import Data.Repr (fromMap) as Repr
import Data.Map.Extra (stringifyKeys) as Map

import Record.Extra (keys) as Record

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)

import Noodle.Id (InputR, OutputR)
import Noodle.Stateful (class StatefulM)

import Noodle.Fn.Raw.Protocol as Raw
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr
type Tracker  state (is :: Row Type) (os :: Row Type) repr = Raw.Tracker  state repr


type PreUpdatesRow  state (is :: Row Type) (os :: Row Type) repr = Raw.PreUpdatesRow  state repr
type PostUpdatesRow state (is :: Row Type) (os :: Row Type) repr = Raw.PostUpdatesRow state repr
type FocusedUpdate  state (is :: Row Type) (os :: Row Type) repr = Raw.FocusedUpdate  state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InputR repr
    -> Map OutputR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make


inputs :: forall state is os repr. Tracker state is os repr -> Effect (Map InputR repr)
inputs = Raw.inputs


outputs :: forall state is os repr. Tracker state is os repr-> Effect (Map OutputR repr)
outputs = Raw.outputs


lastInput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe InputR)
lastInput = Raw.lastInput


lastOutput :: forall state is os repr. Tracker state is os repr -> Effect (Maybe OutputR)
lastOutput = Raw.lastOutput


inputsRec :: forall state is isrl os repr. RL.RowToList is isrl => FromReprRow isrl is repr () is => Tracker state is os repr -> Effect (Record is)
inputsRec tracker = Raw.inputs tracker <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap


outputsRec :: forall state is os osrl repr. RL.RowToList os osrl => FromReprRow osrl os repr () os => Tracker state is os repr -> Effect (Record os)
outputsRec tracker = Raw.outputs tracker <#> map Repr <#> Map.stringifyKeys reflect' <#> Repr.fromMap