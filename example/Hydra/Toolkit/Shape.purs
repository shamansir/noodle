module Hydra.Toolkit.Shape where


import Prelude (($))

import Hydra (Hydra(..), Value(..))
import Hydra as Hydra

import Noodle.Channel.Shape (Shape')
import Noodle.Channel.Shape as Channel


value :: Shape' Hydra
value =
  Channel.shape''' (Value' $ Num 0.0) isValue
  where isValue (Value' _) = true
        isValue _ = false


osc :: Shape' Hydra
osc =
  Channel.shape''' Hydra.defaultOsc isOsc
  where isOsc (Osc _ _ _) = true
        isOsc _ = false