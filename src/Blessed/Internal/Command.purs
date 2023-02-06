module Blessed.Internal.Command where

import Prelude ((>>>))

import Foreign.JSON
import Foreign (Foreign)

import Data.Codec (Codec) as CA
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Type.Data.Symbol (class IsSymbol)

import Blessed.Internal.JsApi (HandlerCallEnc, SNode)


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


arg :: forall m a value d. CA.Codec m a Json value d → value → Json
arg = CA.encode


withProcess :: String -> Array Json -> Command
withProcess cmd args =
    WithProcess { cmd, args }


data NodeOrJson state
    = NodeArg (SNode state)
    | JsonArg Json


node :: forall state. SNode state -> NodeOrJson state
node = NodeArg


narg :: forall m a value d state. CA.Codec m a Json value d → value → NodeOrJson state
narg codec = arg codec >>> JsonArg