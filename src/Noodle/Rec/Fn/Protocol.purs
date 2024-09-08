module Noodle.Rec.Fn.Protocol
  ( Protocol
  , make
  , getState
  , getInlets, getOutlets
  , sendOut, sendIn
  , modifyState
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude

import Record (set) as Record
import Type.Proxy (Proxy(..))

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Noodle.Id (Inlet, Outlet, inletR, outletR)
import Noodle.Node.Has (class HasInlet, class HasOutlet)
import Noodle.Rec.Fn.Tracker (Tracker)
import Noodle.Fn.Generic.Protocol as Generic


type Protocol state (is :: Row Type) (os :: Row Type) = Generic.Protocol state (Record is) (Record os)


make
    :: forall state (is :: Row Type) (os :: Row Type) m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os /\ Protocol state is os)
make = Generic.make


getState :: forall state is os. Protocol state is os -> Effect state
getState p = p.getState unit


getInlets :: forall state is os. Protocol state is os -> Effect (Record is)
getInlets p = p.getInlets unit <#> Tuple.snd


getOutlets :: forall state is os. Protocol state is os -> Effect (Record os)
getOutlets p = p.getOutlets unit <#> Tuple.snd


sendOut :: forall o state is os os' m dout. MonadEffect m => HasOutlet os os' o dout => Outlet o -> dout -> Protocol state is os -> Effect Unit
sendOut outlet dout =
    Generic._modifyOutlet (Record.set (Proxy :: _ o) dout) $ outletR outlet


sendIn :: forall i state is is' os m din. MonadEffect m => HasInlet is is' i din => Inlet i -> din -> Protocol state is os -> Effect Unit
sendIn inlet din =
    Generic._modifyInlet (Record.set (Proxy :: _ i) din) $ inletR inlet


modifyState :: forall state is os. (state -> state) -> Protocol state is os -> Effect Unit
modifyState = Generic._modifyState
