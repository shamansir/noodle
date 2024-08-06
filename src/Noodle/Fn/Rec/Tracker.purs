module Noodle.Fn.Rec.Tracker where

import Prelude ((<#>))

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.SProxy (proxify)
import Data.Symbol (class IsSymbol)

import Prim.Row (class Cons) as Row
import Record (get) as Record

import Noodle.Id (Input, InletR, Output, OutletR)
import Noodle.Fn.Generic.Tracker (Tracker, inlets, outlets, lastInput, lastOutput) as Generic


type Tracker  state (is :: Row Type) (os :: Row Type) = Generic.Tracker  state (Record is) (Record os)


inlets :: forall state is os. Tracker state is os -> Effect (Record is)
inlets = Generic.inlets


outlets :: forall state is os. Tracker state is os -> Effect (Record os)
outlets = Generic.outlets


lastInput :: forall state is os. Tracker state is os -> Effect (Maybe InletR)
lastInput = Generic.lastInput


lastOutput :: forall state is os. Tracker state is os -> Effect (Maybe OutletR)
lastOutput = Generic.lastOutput


atInput :: forall i is os state din. Row.Cons i din is is => IsSymbol i => Input i -> Tracker state is os -> Effect din
atInput input tracker = inlets tracker <#> Record.get (proxify input)


atOutput :: forall o is os state dout. Row.Cons o dout os os => IsSymbol o => Output o -> Tracker state is os -> Effect dout
atOutput output tracker = outlets tracker <#> Record.get (proxify output)