module Hydra.Toolkit.Shape where


import Prelude (($))

import Hydra (Hydra(..), Value(..))
import Hydra as Hydra

import Noodle.Channel.Shape (Shape')
import Noodle.Channel.Shape as Channel


value :: Shape' Hydra
value =
  Channel.shapeBy "value" None Hydra.isValue


entity :: Shape' Hydra
entity =
  Channel.shapeBy "entity" None Hydra.isEntity


out :: Shape' Hydra
out =
  Channel.shapeBy "out" None Hydra.isOut