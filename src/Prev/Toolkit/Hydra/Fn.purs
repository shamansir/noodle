module Prev.Toolkit.Hydra.Fn
  ( noise
  )
  where


import Prev.Toolkit.Hydra.Op
import Prev.Toolkit.Hydra.Fn.Gen as Gen

import Prev.Noodle.Fn (Fn, class ToFn) as Noodle
import Prev.Noodle.Fn (make) as Fn
import Prev.Noodle.Fn.Process (receive, send, sendIn) as Fn


import Prelude

import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)
import Data.Vec (Vec, (!!), (+>))
import Data.Vec (fromArray, toArray, zipWithE, singleton, empty) as Vec
import Data.Typelevel.Num.Reps (D0, D1, D2, D3, D4, D5, D6, d0, d1, d2, d3, d4, d5, d6)



noise :: Gen.Fn
noise =
    Gen.fn2v
        "noise"
        (  "scale"  /\ Num 10.0
        +> "offset" /\ Num 0.1
        +> Vec.empty
        )
        ( Vec.singleton "offset" )
        (\scale offset -> Noise { scale, offset })
