module Rpd.UUID where

import Prelude (Unit, unit)
import Effect (Effect)


-- foreign import new :: Unit -> Effect String


foreign import new :: Effect String


-- new :: Effect String
-- new = new_ unit
