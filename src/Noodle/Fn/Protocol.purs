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
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class ToChRepr, class ToChReprRow, class FromChReprRow)
import Noodle.Repr.ChRepr (ensureTo, unwrap) as ChRepr


type Protocol state (is :: Row Type) (os :: Row Type) chrepr = Raw.Protocol state chrepr


make
    :: forall state (is :: Row Type) (os :: Row Type) chrepr m
    .  MonadEffect m
    => state
    -> Map InletR chrepr
    -> Map OutletR chrepr
    -> m (Tracker state is os chrepr /\ Protocol state is os chrepr)
make = Raw.make


makeRec
    :: forall state (is :: Row Type) isrl (os :: Row Type) osrl chrepr m
    .  MonadEffect m
    => ToChReprRow isrl is InletR chrepr
    => ToChReprRow osrl os OutletR chrepr
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os chrepr /\ Protocol state is os chrepr)
makeRec state is os = make state (fromRec inletR is) (fromRec outletR os)


getState :: forall state is os chrepr. Protocol state is os chrepr -> Effect state
getState = Raw.getState


getInlets :: forall state is os chrepr. Protocol state is os chrepr -> Effect (Map InletR chrepr)
getInlets = Raw.getInlets


getOutlets :: forall state is os chrepr. Protocol state is os chrepr -> Effect (Map OutletR chrepr)
getOutlets = Raw.getOutlets


getRecInlets :: forall state is isrl os chrepr. FromChReprRow isrl is chrepr => Protocol state is os chrepr -> Effect (Record is)
getRecInlets p = p.getInlets unit <#> Tuple.snd <#> toRec Id.inletRName


getRecOutlets :: forall state is os osrl chrepr. FromChReprRow osrl os chrepr => Protocol state is os chrepr -> Effect (Record os)
getRecOutlets p = p.getOutlets unit <#> Tuple.snd <#> toRec Id.outletRName


modifyState :: forall state is os chrepr. (state -> state) -> Protocol state is os chrepr -> Effect Unit
modifyState = Raw.modifyState


-- private: doesn't check if outlet is in `os`
_sendOut :: forall o state is os dout chrepr. IsSymbol o => HasFallback chrepr => ToChRepr dout chrepr => Outlet o -> dout -> Protocol state is os chrepr -> Effect Unit
_sendOut outlet = Raw.sendOut (outletR outlet) <<< ChRepr.unwrap <<< ChRepr.ensureTo


-- private: doesn't check if inlet is in `is`
_sendIn :: forall i state is os dout chrepr. IsSymbol i => HasFallback chrepr => ToChRepr dout chrepr => Inlet i -> dout -> Protocol state is os chrepr -> Effect Unit
_sendIn inlet = Raw.sendIn (inletR inlet) <<< ChRepr.unwrap <<< ChRepr.ensureTo


_unsafeSendOut :: forall state is os chrepr. OutletR -> chrepr -> Protocol state is os chrepr -> Effect Unit
_unsafeSendOut = Raw.sendOut


_unsafeSendIn :: forall state is os chrepr. InletR -> chrepr -> Protocol state is os chrepr -> Effect Unit
_unsafeSendIn = Raw.sendIn