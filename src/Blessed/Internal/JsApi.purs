module Blessed.Internal.JsApi where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (class Newtype)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA

import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.Emitter as E


type Registry = Map NK.RawNodeKey Unit
newtype SRegistry = SRegistry Json
newtype EventJson = EventJson Json



derive instance Newtype SRegistry _
derive instance Newtype EventJson _

derive newtype instance EncodeJson SRegistry
derive newtype instance EncodeJson EventJson


-- encode state to Json as well?
data SProp = SProp String Json
data SHandler s = SHandler E.EventId (Array Json) (Ref s -> NK.RawNodeKey -> EventJson -> Effect Unit)
data SNode s = SNode NK.RawNodeKey (Array SProp) (Array (SNode s)) (Array (SHandler s))


newRegistry :: SRegistry
newRegistry = SRegistry $ CA.encode CA.null unit


unveilRegistry :: SRegistry -> Registry
unveilRegistry = const $ Map.empty


unwrapProp ∷ SProp → String /\ Json
unwrapProp (SProp name json) = name /\ json


{-
data NodeId_ (x :: Subject) (sym :: Symbol)

foreign import data RawNodeKey :: forall (x :: Subject) (sym :: Symbol). Proxy sym -> Proxy x -> NodeId_ x sym


reflectNodeId :: forall x sym. NodeId_ x sym -> NodeId
reflectNodeId _ = NodeId ((_ :: x) /\ reflectSymbol (Proxy :: _ sym))
-}



newtype HandlerCallEnc =
    HandlerCallEnc
        { marker :: String
        , nodeId :: String
        , nodeSubj :: String
        , event :: String
        , eventUniqueId :: String
        , index :: String
        , args :: Array Json
        , call :: EventJson -> Effect Unit
        }

newtype HandlerRefEnc =
    HandlerRefEnc
        { marker :: String
        , nodeId :: String
        , nodeSubj :: String
        , event :: String
        , eventUniqueId :: String
        , index :: String
        }

derive instance Newtype HandlerCallEnc _
derive instance Newtype HandlerRefEnc _

newtype BlessedEnc =
    BlessedEnc
        { marker :: String
        , root :: Json
        , handlersFns :: Array HandlerCallEnc
        }


type PropJson =
    { name :: String, value :: Json }


newtype NodeEnc =
    NodeEnc
        { marker :: String
        , nodeSubj :: String
        , nodeId :: String
        , props :: Array PropJson
        , children :: Array NodeEnc
        , handlers :: Array HandlerRefEnc
        , parent :: Maybe String
        }

derive instance Newtype NodeEnc _


data CommandType
    = Call
    | Get
    | Set
    | Sub
    | Process


newtype CallCommandEnc =
    CallCommandEnc
        { marker :: String
        , type :: String
        , method :: String
        , args :: Array Json
        }


newtype GetCommandEnc =
    GetCommandEnc
        { marker :: String
        , type :: String
        , property :: String
        }


newtype GetPCommandEnc =
    GetPCommandEnc
        { marker :: String
        , type :: String
        , path :: Array String
        }


newtype SetCommandEnc =
    SetCommandEnc
        { marker :: String
        , type :: String
        , property :: String
        , value :: Json
        }


newtype SetPCommandEnc =
    SetPCommandEnc
        { marker :: String
        , type :: String
        , path :: Array String
        , value :: Json
        }


newtype SubCommandEnc =
    SubCommandEnc
        { marker :: String
        , type :: String
        , event :: String
        , args :: Array Json
        -- , handler :: HandlerRefEnc
        }


newtype ProcessCommandEnc =
    ProcessCommandEnc
        { marker :: String
        , type :: String
        , method :: String
        , args :: Array Json
        }


newtype CommandEnc =
    CommandEnc Json


derive instance Newtype CallCommandEnc _
derive instance Newtype GetCommandEnc _
derive instance Newtype SetCommandEnc _
derive instance Newtype GetPCommandEnc _
derive instance Newtype SetPCommandEnc _
derive instance Newtype SubCommandEnc _
derive instance Newtype ProcessCommandEnc _
derive instance Newtype CommandEnc _