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
-- import Noodle.Repr.ChRepr (class ToChRepr, class ToChReprRow, class FromChReprRow)
-- import Noodle.Repr.ChRepr (ensureTo, unwrap) as ChRepr
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel, class FromValuesInChannelRow, class ToValuesInChannelRow)
import Noodle.Repr.ValueInChannel (accept, fromValueInChannel) as ViC


type Protocol state (is :: Row Type) (os :: Row Type) chrepr = Raw.Protocol state chrepr


make
    :: forall state (is :: Row Type) (os :: Row Type) chrepr m
    .  MonadEffect m
    => state
    -> Map InletR  chrepr
    -> Map OutletR chrepr
    -> m (Tracker state is os chrepr /\ Protocol state is os chrepr)
make = Raw.make


makeRec
    :: forall state (is :: Row Type) isrl (os :: Row Type) osrl chrepr m
    .  MonadEffect m
    => FromValuesInChannelRow isrl is InletR chrepr
    => FromValuesInChannelRow osrl os OutletR chrepr
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os chrepr /\ Protocol state is os chrepr)
makeRec state is os =
    Raw.make' state (fromRec inletR is) (fromRec outletR os)


getState :: forall state is os chrepr. Protocol state is os chrepr -> Effect state
getState = Raw.getState


getInlets :: forall state is os chrepr. Protocol state is os chrepr -> Effect (Map InletR (ValueInChannel chrepr))
getInlets = Raw.getInlets


getOutlets :: forall state is os chrepr. Protocol state is os chrepr -> Effect (Map OutletR (ValueInChannel chrepr))
getOutlets = Raw.getOutlets


getRecInlets :: forall state is vis visrl os chrepr. ToValuesInChannelRow visrl vis chrepr => Protocol state is os chrepr -> Effect (Record vis)
getRecInlets p = p.getInlets unit <#> Tuple.snd <#> toRec Id.inletRName


getRecOutlets :: forall state is os vos vosrl chrepr. ToValuesInChannelRow vosrl vos chrepr => Protocol state is os chrepr -> Effect (Record vos)
getRecOutlets p = p.getOutlets unit <#> Tuple.snd <#> toRec Id.outletRName


modifyState :: forall state is os chrepr. (state -> state) -> Protocol state is os chrepr -> Effect Unit
modifyState = Raw.modifyState


-- private: doesn't check if outlet is in `os`
_sendOut :: forall o state is os dout chrepr. IsSymbol o => FromValueInChannel dout chrepr => Outlet o -> dout -> Protocol state is os chrepr -> Effect Unit
_sendOut outlet = Raw.sendOut (outletR outlet) <<< ViC.accept <<< ViC.fromValueInChannel


-- private: doesn't check if inlet is in `is`
_sendIn :: forall i state is os din chrepr. IsSymbol i => FromValueInChannel din chrepr => Inlet i -> din -> Protocol state is os chrepr -> Effect Unit
_sendIn inlet = Raw.sendIn (inletR inlet) <<< ViC.accept <<< ViC.fromValueInChannel


_unsafeSendOut :: forall state is os chrepr. OutletR -> ValueInChannel chrepr -> Protocol state is os chrepr -> Effect Unit
_unsafeSendOut = Raw.sendOut


_unsafeSendIn :: forall state is os chrepr. InletR -> ValueInChannel chrepr -> Protocol state is os chrepr -> Effect Unit
_unsafeSendIn = Raw.sendIn