module Noodle.Fn.Rec.Tracker where

import Prelude ((<#>))

import Effect (Effect)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)

import Record (get) as Record

import Noodle.Id (Inlet, InletR, Outlet, OutletR)
import Noodle.Fn.Generic.Tracker (Tracker, inlets, outlets, lastInlet, lastOutlet) as Generic
import Noodle.Node.Has (class HasInlet, class HasOutlet)


type Tracker state (is :: Row Type) (os :: Row Type) = Generic.Tracker state (Record is) (Record os)


inlets :: forall state is os. Tracker state is os -> Effect (Record is)
inlets = Generic.inlets


outlets :: forall state is os. Tracker state is os -> Effect (Record os)
outlets = Generic.outlets


lastInlet :: forall state is os. Tracker state is os -> Effect (Maybe InletR)
lastInlet = Generic.lastInlet


lastOutlet :: forall state is os. Tracker state is os -> Effect (Maybe OutletR)
lastOutlet = Generic.lastOutlet


atInlet :: forall i is is' os state din. HasInlet is is' i din => IsSymbol i => Inlet i -> Tracker state is os -> Effect din
atInlet _ tracker = inlets tracker <#> Record.get (Proxy :: _ i)


atOutlet :: forall o is os os' state dout. HasOutlet os os' o dout => IsSymbol o => Outlet o -> Tracker state is os -> Effect dout
atOutlet _ tracker = outlets tracker <#> Record.get (Proxy :: _ o)