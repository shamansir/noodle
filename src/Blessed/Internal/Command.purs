module Blessed.Internal.Command where

import Foreign.JSON
import Foreign (Foreign)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Type.Data.Symbol (class IsSymbol)
import Blessed.Internal.JsApi (HandlerCallEnc)


{- data CommandType
    = Call
    | Set
    | Get
    | WithProcess -}


data Command
    = Call { cmd :: String, args :: Array Json }
    | CallEx { cmd :: String, args :: Array Json, handlers :: Array HandlerCallEnc }
    | Set { prop :: String, value :: Json }
    | Get { prop :: String }
    -- | Global { }
    | WithProcess { cmd :: String, args :: Array Json }


call :: String -> Array Json -> Command
call cmd args = Call { cmd, args }


callEx :: String -> Array Json -> Array HandlerCallEnc -> Command
callEx cmd args handlers = CallEx { cmd, args, handlers }


set :: String -> Json -> Command
set prop value = Set { prop, value }


get :: String -> Command
get prop = Get { prop }


arg = CA.encode


withProcess :: String -> Array Json -> Command
withProcess cmd args =
    WithProcess { cmd, args }