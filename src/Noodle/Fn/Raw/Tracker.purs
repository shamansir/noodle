module Noodle.Fn.Raw.Tracker where

import Prelude ((<#>))

import Data.Map (Map)
import Data.Map (lookup) as Map

import Effect (Effect)

import Data.Maybe (Maybe)

import Noodle.Id (InletR, OutletR)
import Noodle.Fn.Generic.Tracker (Tracker, inlets, outlets, lastInput, lastOutput) as Generic


type Tracker state repr = Generic.Tracker state (Map InletR repr) (Map OutletR repr)


inlets :: forall state repr. Tracker state repr -> Effect (Map InletR repr)
inlets = Generic.inlets


outlets :: forall state repr. Tracker state repr -> Effect (Map OutletR repr)
outlets = Generic.outlets


lastInput :: forall state repr. Tracker state repr -> Effect (Maybe InletR)
lastInput = Generic.lastInput


lastOutput :: forall state repr. Tracker state repr -> Effect (Maybe OutletR)
lastOutput = Generic.lastOutput


atInput :: forall state repr. InletR -> Tracker state repr -> Effect (Maybe repr)
atInput input tracker = inlets tracker <#> Map.lookup input


atOutput :: forall state repr. OutletR -> Tracker state repr -> Effect (Maybe repr)
atOutput output tracker = outlets tracker <#> Map.lookup output