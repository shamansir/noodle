module Hydra.Toolkit where


import Prelude ((<>), ($), (<$>))

import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (make) as T

import Data.Tuple.Nested ((/\))


import Hydra (Hydra)
import Hydra (default) as Hydra

import Hydra.Toolkit.Node as Node
import Hydra.Toolkit.Generate as Gen


toolkit :: Toolkit Hydra
toolkit =
  T.make Hydra.default $
    [ "num" /\ Node.number
    , "time" /\ Node.time
    , "mouse" /\ Node.mouse
    , "pi" /\ Node.pi
    , "seq" /\ Node.seq
    , "palette" /\ Node.palette
    , "solid-pal" /\ Node.solidPalette
    ] <> (Gen.generate <$> Gen.all) <>
    [ "out" /\ Node.out
    , "render" /\ Node.render
    , "from-buffer" /\ Node.fromBuffer
    , "to-buffer" /\ Node.toBuffer
    , "math" /\ Node.math
    ]
