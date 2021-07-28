module Hydra.Engine where


import Prelude (Unit)

import Effect (Effect)


foreign import init :: String -> Effect Unit


foreign import evaluate :: String -> Effect Unit