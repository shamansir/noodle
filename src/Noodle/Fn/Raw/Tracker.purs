module Noodle.Fn.Raw.Tracker where

import Data.Map (Map)

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InputR, OutputR)
import Noodle.Fn.Generic.Tracker (Tracker, inputs, outputs, lastInput, lastOutput) as Generic


type Tracker state repr = Generic.Tracker state (Map InputR repr) (Map OutputR repr)


inputs :: forall state repr. Tracker state repr -> Effect (Map InputR repr)
inputs = Generic.inputs


outputs :: forall state repr. Tracker state repr -> Effect (Map OutputR repr)
outputs = Generic.outputs


lastInput :: forall state repr. Tracker state repr -> Effect (Maybe InputR)
lastInput = Generic.lastInput


lastOutput :: forall state repr. Tracker state repr -> Effect (Maybe OutputR)
lastOutput = Generic.lastOutput