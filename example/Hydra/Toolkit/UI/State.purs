module Hydra.Toolkit.UI.State where


import Prelude (Unit, unit)


import Hydra (Buffer)
import Hydra.Queue (Queue)
import Hydra.Queue as Queue


type State = Queue


init :: State
init = Queue.empty