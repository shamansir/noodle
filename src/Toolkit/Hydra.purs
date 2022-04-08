module Toolkit.Hydra where

import Prelude

import Toolkit.Hydra.Op (Hydra(..))
import Toolkit.Hydra.Fn as Fn
import Toolkit.Hydra.Fn.Gen (toNodeFn) as Fn


import Noodle.Toolkit (Toolkit, empty, registerFn) as T

type Toolkit = T.Toolkit Unit Hydra


toolkit :: Toolkit
toolkit =
    T.registerFn
        (T.empty None)
        (Fn.toNodeFn Fn.noise)