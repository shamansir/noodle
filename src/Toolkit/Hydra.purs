module Toolkit.Hydra.Toolkit where

import Prelude

import Toolkit.Hydra.Op
import Toolkit.Hydra.Fn as Fn


import Noodle.Toolkit (Toolkit, empty, registerFn) as T

type Toolkit = T.Toolkit Unit Hydra


toolkit :: Toolkit
toolkit =
    T.registerFn
        (T.empty None)
        ?wh
        -- Fn.noise