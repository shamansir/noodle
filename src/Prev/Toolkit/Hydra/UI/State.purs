module Prev.Toolkit.Hydra.UI.State where

import Prelude (Unit, unit)


import Prev.Toolkit.Hydra.Op (Buffer)
import Prev.Toolkit.Hydra.Queue (Queue)
import Prev.Toolkit.Hydra.Queue as Queue


type State = Queue


init :: State
init = Queue.empty
