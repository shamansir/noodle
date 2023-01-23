module Blessed.Internal.JsApi where

import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (class Newtype)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA

import Blessed.Internal.BlessedKind as K



-- newtype NodeId (kind :: NKind) = NodeId String
newtype NodeId = NodeId String
newtype EventId = EventId String


type Registry = Map NodeId Json
newtype SRegistry = SRegistry Json
newtype EventJson = EventJson Json



derive instance Newtype NodeId _
derive instance Newtype EventId _
derive instance Newtype SRegistry _
derive instance Newtype EventJson _

derive newtype instance EncodeJson NodeId
derive newtype instance EncodeJson EventId
derive newtype instance EncodeJson SRegistry
derive newtype instance EncodeJson EventJson




data SProp = SProp String Json
data SHandler = SHandler EventId (Array Json) (SRegistry -> K.NKind -> NodeId -> EventJson -> Effect Unit)
data SNode = SNode K.NKind NodeId (Array SProp) (Array SNode) (Array SHandler)


newRegistry :: SRegistry
newRegistry = SRegistry $ CA.encode CA.null unit


unveilRegistry :: SRegistry -> Registry
unveilRegistry = const $ Map.empty


unwrapProp ∷ SProp → String /\ Json
unwrapProp (SProp name json) = name /\ json


{-
data NodeId_ (x :: NKind) (sym :: Symbol)

foreign import data NodeId' :: forall (x :: NKind) (sym :: Symbol). Proxy sym -> Proxy x -> NodeId_ x sym


reflectNodeId :: forall x sym. NodeId_ x sym -> NodeId
reflectNodeId _ = NodeId ((_ :: x) /\ reflectSymbol (Proxy :: _ sym))
-}



newtype HandlerCallEnc =
    HandlerCallEnc
        { nodeId :: String
        , nodeKind :: String
        , event :: String
        , index :: String
        , args :: Array Json
        , call :: EventJson -> Effect Unit
        }

newtype HandlerRefEnc =
    HandlerRefEnc
        { nodeId :: String
        , nodeKind :: String
        , event :: String
        , args :: Array Json
        , index :: String
        }

derive instance Newtype HandlerCallEnc _
derive instance Newtype HandlerRefEnc _

newtype BlessedEnc =
    BlessedEnc
        { root :: Json
        , handlersFns :: Array HandlerCallEnc
        }


type PropJson =
    { name :: String, value :: Json }


newtype NodeEnc =
    NodeEnc
        { nodeKind :: String
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
    | Process


newtype CallCommandEnc =
    CallCommandEnc
        { type :: String
        , method :: String
        , args :: Array Json
        }


newtype GetCommandEnc =
    GetCommandEnc
        { type :: String
        , property :: String
        }


newtype SetCommandEnc =
    SetCommandEnc
        { type :: String
        , property :: String
        , value :: Json
        }

newtype ProcessCommandEnc =
    ProcessCommandEnc
        { type :: String
        , method :: String
        , args :: Array Json
        }


newtype CommandEnc =
    CommandEnc Json


newtype CallDump =
    CallDump
        { nodeId :: String
        , nodeKind :: String
        , event :: String
        , args :: Array Json
        }


derive instance Newtype CallCommandEnc _
derive instance Newtype GetCommandEnc _
derive instance Newtype SetCommandEnc _
derive instance Newtype ProcessCommandEnc _
derive instance Newtype CommandEnc _
derive instance Newtype CallDump _