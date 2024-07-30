module Noodle.Fn.Rec.Tracker where

import Prelude ((<#>))

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.SProxy (proxify)
import Data.Symbol (class IsSymbol)

import Prim.Row (class Cons) as Row
import Record (get) as Record

import Noodle.Id (Input, InputR, Output, OutputR)
import Noodle.Fn.Generic.Tracker (Tracker, inputs, outputs, lastInput, lastOutput) as Generic


type Tracker  state (is :: Row Type) (os :: Row Type) = Generic.Tracker  state (Record is) (Record os)


inputs :: forall state is os. Tracker state is os -> Effect (Record is)
inputs = Generic.inputs


outputs :: forall state is os. Tracker state is os -> Effect (Record os)
outputs = Generic.outputs


lastInput :: forall state is os. Tracker state is os -> Effect (Maybe InputR)
lastInput = Generic.lastInput


lastOutput :: forall state is os. Tracker state is os -> Effect (Maybe OutputR)
lastOutput = Generic.lastOutput


atInput :: forall i is os state din. Row.Cons i din is is => IsSymbol i => Input i -> Tracker state is os -> Effect din
atInput input tracker = inputs tracker <#> Record.get (proxify input)


atOutput :: forall o is os state dout. Row.Cons o dout os os => IsSymbol o => Output o -> Tracker state is os -> Effect dout
atOutput output tracker = outputs tracker <#> Record.get (proxify output)