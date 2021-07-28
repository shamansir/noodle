module Hydra.Toolkit where


import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (make) as T

import Data.Tuple.Nested ((/\))


import Hydra (Hydra)
import Hydra (default) as Hydra

import Hydra.Toolkit.Node as Node


toolkit :: Toolkit Hydra
toolkit =
  T.make Hydra.default
    [ "num" /\ Node.number
    , "osc" /\ Node.osc
    , "out" /\ Node.out
    ]
