module Blessed.Internal.JsApi where

import Prelude

import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


newtype NodeId = NodeId String
newtype EventId = EventId String


type Registry = Map NodeId Json
newtype SRegistry = SRegistry Json
newtype EventJson = EventJson Json


data Kind
    = Box
    | Screen


data SProp = SProp String Json
data SHandler = SHandler EventId (SRegistry -> NodeId -> EventJson -> Effect Unit)
data SNode = SNode Kind NodeId (Array SProp) (Array SNode) (Array SHandler)


newRegistry :: SRegistry
newRegistry = SRegistry $ CA.encode CA.null unit


unveilRegistry :: SRegistry -> Registry
unveilRegistry = const $ Map.empty
