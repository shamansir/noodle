module Noodle.Fn.Protocol
  ( Protocol
  , make, makeRec
  , getState, modifyState
  , getInlets, getOutlets
  , getRecInlets, getRecOutlets
  , _sendOut, _unsafeSendOut
  , _sendIn, _unsafeSendIn
  )
  where


import Prelude

import Data.Map (Map)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (InletR, OutletR, Inlet, Outlet, outletR, inletR)
import Noodle.Id (inletRName, outletRName) as Id

import Noodle.Raw.Fn.Protocol as Raw
-- TODO: import Noodle.Raw.Fn.Protocol as RawExports
import Noodle.Fn.Tracker (Tracker)
import Noodle.Raw.FromToRec (toRec, fromRec)

import Noodle.Repr (class ToRepr, class ToReprRow, class FromReprRow)
import Noodle.Repr (ensureTo, unwrap) as Repr


type Protocol state (is :: Row Type) (os :: Row Type) repr = Raw.Protocol state repr


make
    :: forall state (is :: Row Type) (os :: Row Type) repr m
    .  MonadEffect m
    => state
    -> Map InletR repr
    -> Map OutletR repr
    -> m (Tracker state is os repr /\ Protocol state is os repr)
make = Raw.make


makeRec
    :: forall state (is :: Row Type) isrl (os :: Row Type) osrl repr m
    .  MonadEffect m
    => ToReprRow isrl is InletR repr
    => ToReprRow osrl os OutletR repr
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os repr /\ Protocol state is os repr)
makeRec state is os = make state (fromRec inletR is) (fromRec outletR os)


getState :: forall state is os repr. Protocol state is os repr -> Effect state
getState = Raw.getState


getInlets :: forall state is os repr. Protocol state is os repr -> Effect (Map InletR repr)
getInlets = Raw.getInlets


getOutlets :: forall state is os repr. Protocol state is os repr -> Effect (Map OutletR repr)
getOutlets = Raw.getOutlets


getRecInlets :: forall state is isrl os repr. FromReprRow isrl is repr => Protocol state is os repr -> Effect (Record is)
getRecInlets p = p.getInlets unit <#> Tuple.snd <#> toRec Id.inletRName


getRecOutlets :: forall state is os osrl repr. FromReprRow osrl os repr => Protocol state is os repr -> Effect (Record os)
getRecOutlets p = p.getOutlets unit <#> Tuple.snd <#> toRec Id.outletRName


modifyState :: forall state is os repr. (state -> state) -> Protocol state is os repr -> Effect Unit
modifyState = Raw.modifyState


-- private: doesn't check if outlet is in `os`
_sendOut :: forall o state is os dout repr. IsSymbol o => ToRepr dout repr => Outlet o -> dout -> Protocol state is os repr -> Effect Unit
_sendOut outlet = Raw.sendOut (outletR outlet) <<< Repr.unwrap <<< Repr.ensureTo


-- private: doesn't check if inlet is in `is`
_sendIn :: forall i state is os dout repr. IsSymbol i => ToRepr dout repr => Inlet i -> dout -> Protocol state is os repr -> Effect Unit
_sendIn inlet = Raw.sendIn (inletR inlet) <<< Repr.unwrap <<< Repr.ensureTo



_unsafeSendOut :: forall state is os repr. OutletR -> repr -> Protocol state is os repr -> Effect Unit
_unsafeSendOut = Raw.sendOut


_unsafeSendIn :: forall state is os repr. InletR -> repr -> Protocol state is os repr -> Effect Unit
_unsafeSendIn = Raw.sendIn