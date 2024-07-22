module Noodle.Fn.Rec.Tracker where

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InputR, OutputR)
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