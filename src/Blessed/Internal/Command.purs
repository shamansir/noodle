module Blessed.Internal.Command where

import Prelude ((>>>))

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
    | Get { prop :: String }
    | GetP { path :: Array String }
    | Set { prop :: String, value :: Json }
    | SetP { path :: Array String, value :: Json }
    | Sub { event :: String, args :: Array Json, handler :: HandlerCallEnc }
    | WithProcess { cmd :: String, args :: Array Json }


call :: String -> Array Json -> Command
call cmd args = Call { cmd, args }


callEx :: String -> Array Json -> Array HandlerCallEnc -> Command
callEx cmd args handlers = CallEx { cmd, args, handlers }


set :: String -> Json -> Command
set prop value = Set { prop, value }


get :: String -> Command
get prop = Get { prop }


setP :: Array String -> Json -> Command
setP path value = SetP { path, value }


getP :: Array String -> Command
getP path = GetP { path }


sub :: String -> Array Json -> HandlerCallEnc -> Command
sub event args handler = Sub { event, args, handler }


arg :: forall m a value d. CA.Codec m a Json value d → value → Json
arg = CA.encode


withProcess :: String -> Array Json -> Command
withProcess cmd args =
    WithProcess { cmd, args }


data NodeOrJson state
    = NodeArg (SNode state)
    -- TODO: by Key as well
    | JsonArg Json


node :: forall state. SNode state -> NodeOrJson state
node = NodeArg


narg :: forall m a value d state. CA.Codec m a Json value d → value → NodeOrJson state
narg codec = arg codec >>> JsonArg