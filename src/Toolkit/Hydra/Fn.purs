module Toolkit.Hydra.Fn
  ( Evaluate(..)
  , FnTV
  , FnV
  )
  where


import Toolkit.Hydra

import Noodle.Fn (Fn, class ToFn) as Noodle
import Noodle.Fn (make) as Fn
import Noodle.Fn.Process (receive, send, sendIn) as Fn


import Prelude

import Data.Maybe (Maybe(..))
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)


data Evaluate a = Evaluate Hydra a


type FnV target = Noodle.Fn String Value String target Unit Evaluate target


type FnTV target = Noodle.Fn String TextureOrValue String target Unit Evaluate target



noise :: FnV Source
noise = Fn.make "noise"
    [ "scale" /\ Num 0.0, "offset" /\ Num 0.0 ]
    [ "noise" /\ Noise { scale : Num 0.0, offset : Num 0.0 } ]
    $ pure unit
