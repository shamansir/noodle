module Toolkit.Hydra.Toolkit where

import Prelude

import Toolkit.Hydra.Op
import Toolkit.Hydra.Fn


import Noodle.Toolkit (Toolkit, empty, registerFn) as T

type Toolkit = T.Toolkit Unit Hydra


toolkit :: Toolkit
toolkit = T.empty None