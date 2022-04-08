module Toolkit.Hydra.UI.State where

import Prelude (Unit, unit)


import Toolkit.Hydra.Op (Buffer)
import Toolkit.Hydra.Queue (Queue)
import Toolkit.Hydra.Queue as Queue


type State = Queue


init :: State
init = Queue.empty
