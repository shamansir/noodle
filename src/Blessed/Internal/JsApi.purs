module Blessed.Internal.JsApi where

import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (class Newtype)
import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA


newtype NodeId = NodeId String
newtype EventId = EventId String


type Registry = Map NodeId Json
newtype SRegistry = SRegistry Json
newtype EventJson = EventJson Json


data Kind
    = Box
    | Screen


derive instance Newtype NodeId _
derive instance Newtype EventId _
derive instance Newtype SRegistry _
derive instance Newtype EventJson _


data SProp = SProp String Json
data SHandler = SHandler EventId (SRegistry -> NodeId -> EventJson -> Effect Unit)
data SNode = SNode Kind NodeId (Array SProp) (Array SNode) (Array SHandler)


newRegistry :: SRegistry
newRegistry = SRegistry $ CA.encode CA.null unit


unveilRegistry :: SRegistry -> Registry
unveilRegistry = const $ Map.empty


unwrapProp ∷ SProp → String /\ Json
unwrapProp (SProp name json) = name /\ json


kindFromString ∷ String → Maybe Kind
kindFromString =
    case _ of
        "box" -> Just Box
        "screen" -> Just Screen
        _ -> Nothing


kindToString ∷ Kind → String
kindToString =
    case _ of
        Box -> "box"
        Screen -> "screen"


newtype HandlerCallEnc =
    HandlerCallEnc
        { nodeId :: String
        , event :: String
        , index :: String
        , call :: EventJson -> Effect Unit
        }

newtype HandlerRefEnc =
    HandlerRefEnc
        { nodeId :: String
        , event :: String
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
        { kind :: String
        , nodeId :: String
        , props :: Map String Json
        , children :: Array NodeEnc
        , handlers :: Array HandlerRefEnc
        , parent :: Maybe String
        }

derive instance Newtype NodeEnc _
