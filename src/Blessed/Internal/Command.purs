module Blessed.Command where

import Foreign.JSON
import Foreign (Foreign)


data Command
    = Call { target :: String, cmd :: String, args :: Array Foreign }
    | Set { target :: String, prop :: String, value :: Foreign }