module Blessed.Internal.Command where

import Foreign.JSON
import Foreign (Foreign)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Type.Data.Symbol (class IsSymbol)



newtype NodeId = NodeId String


data Command
    = Call { target :: String, cmd :: String, args :: Array Json }
    | Set { target :: String, prop :: String, value :: Json }
    | Get { target :: String, prop :: String }



-- TODO: cmd names and args count should be checked using row types ?


call :: NodeId -> String -> Array Json -> Command
call (NodeId target) cmd args = Call { target, cmd, args }


set :: NodeId -> String -> Json -> Command
set (NodeId target) prop value = Set { target, prop, value }


get :: NodeId -> String -> Command
get (NodeId target) prop = Get { target, prop }


arg = CA.encode