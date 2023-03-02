module Toolkit.Hydra2 where

import Prelude

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))

import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
-- import Noodle.Id (Family(..), Family', class HasInputs, class HasInputsAt) as Node
-- import Noodle.Id (Input(..), Output(..), InputR) as Fn
-- import Noodle.Id (inputs) as Def
-- import Noodle.Id (reflect', keysToInputsR, keysToOutputsR, reflectInputR, reflectOutputR)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2.Process as Fn
import Noodle.Family.Def as Family

import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Toolkit3.MapsFolds as TMF
import Noodle.Toolkit3.MapsFolds.Repr as TMF

-- type Toolkit = T.Toolkit (Ref Queue, Ref Canvas) Hydra

toolkit =
    Toolkit.from "hydra"
        { foo :
            Family.def
                unit
                { foo : "aaa", bar : "bbb", c : 32 }
                { out : false }
                $ Fn.make "foo" $ pure unit
        }
