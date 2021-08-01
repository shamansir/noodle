module Hydra.Toolkit.Shape where


import Prelude (($))

import Hydra (Hydra(..), Value(..))
import Hydra as Hydra

import Noodle.Channel.Shape (Shape')
import Noodle.Channel.Shape as Channel


value :: Shape' Hydra
value =
  Channel.shape''' None Hydra.isValue


entity :: Shape' Hydra
entity =
  Channel.shape''' None Hydra.isEntity


out :: Shape' Hydra
out =
  Channel.shape''' None Hydra.isOut