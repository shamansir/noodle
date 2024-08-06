module Noodle.Fn.Raw.Tracker where

import Prelude ((<#>))

import Data.Map (Map)
import Data.Map (lookup) as Map

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


atInput :: forall state repr. InputR -> Tracker state repr -> Effect (Maybe repr)
atInput input tracker = inputs tracker <#> Map.lookup input


atOutput :: forall state repr. OutputR -> Tracker state repr -> Effect (Maybe repr)
atOutput output tracker = outputs tracker <#> Map.lookup output