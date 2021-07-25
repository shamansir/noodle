module Hydra.Toolkit where


import Noodle.Toolkit as T

import Hydra (Hydra)
import Hydra as Hydra


toolkit :: T.Toolkit Hydra
toolkit =
  T.make Hydra.default
    [ ]