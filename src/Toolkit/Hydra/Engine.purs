module Tookit.Hydra.Engine where


import Prelude (Unit)

import Effect (Effect)


newtype TargetCanvas = TargetCanvas String

newtype HydraCode = HydraCode String


foreign import init :: TargetCanvas -> Effect Unit


foreign import evaluate :: HydraCode -> Effect Unit