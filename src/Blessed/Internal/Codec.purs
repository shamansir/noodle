module Blessed.Internal.Codec where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Data.Array ((:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Profunctor (wrapIso)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)

import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.JsApi as I


newtype HandlerEnc =
    HandlerEnc
        { nodeId :: String
        , event :: String
        , index :: Int, lindex :: Int, strIndex :: String
        , call :: I.EventJson -> Effect Unit
        }

newtype HandlerRefEnc =
    HandlerRefEnc
        { nodeId :: String
        , event :: String
        , index :: Int, lindex :: Int, strIndex :: String
        }

derive instance Newtype HandlerEnc _
derive instance Newtype HandlerRefEnc _

newtype BlessedEnc = BlessedEnc (Json /\ (Array HandlerEnc))


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


kindCodec :: CA.JsonCodec I.Kind
kindCodec  =
    CA.prismaticCodec "Kind" I.kindFromString I.kindToString CA.string


propertyRecCodec :: CA.JsonCodec PropJson
propertyRecCodec =
    CA.object "OnlyProperty"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


nodeCodec :: CA.JsonCodec NodeEnc
nodeCodec =
    CA.fix \codec ->
        wrapIso NodeEnc $ CAR.object "Node"
            { kind : CA.string
            , nodeId : CA.string
            , props : CAC.map CA.string CA.json
            , children : CA.array codec
            , handlers : CA.array handlerRefCodec
            , parent : CAC.maybe CA.string
            }


handlerRefCodec :: CA.JsonCodec HandlerRefEnc
handlerRefCodec =
    wrapIso HandlerRefEnc $ CA.object "Handler'"
        (CAR.record
            { nodeId : CA.string
            , event : CA.string
            , index : CA.int
            , lindex : CA.int
            , strIndex : CA.string
            }
        )

encode :: I.SNode -> BlessedEnc
encode rootNode =
    case encodeRoot rootNode of
        nodeEnc /\ handlers ->
            BlessedEnc $ (CA.encode nodeCodec nodeEnc) /\ handlers


encodeRoot :: I.SNode -> NodeEnc /\ Array HandlerEnc
encodeRoot = encode' Nothing 0


encode' :: Maybe I.NodeId -> Int -> I.SNode -> NodeEnc /\ Array HandlerEnc
encode'
    maybeParent
    lastHandlerIndex
    (I.SNode kind (I.NodeId nodeId) sprops snodes shandlers)

    =
    -- BlessedEnc (CA.encode CA.null unit /\ [ HandlerEnc { nodeId : "test", event: "test", index : -1, call: const $ Console.log "foo" }])
    nodeEncoded /\ (handlersCalls <> childrenHandlers)
    where

        propsToMap :: Array I.SProp -> Map String Json
        propsToMap = Map.fromFoldable <<< map I.unwrapProp

        encodeHandler :: Int -> Int -> I.SHandler -> HandlerEnc
        encodeHandler totalIndex localIndex (I.SHandler (I.EventId eventId) fn) =
            HandlerEnc
                { nodeId : nodeId
                , event : eventId
                , index : totalIndex
                , lindex : localIndex
                , strIndex : nodeId <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index
                , call : fn (I.newRegistry) (I.NodeId nodeId)
                }

        encodeHandlerRef :: Int -> Int -> I.SHandler -> HandlerRefEnc
        encodeHandlerRef totalIndex localIndex (I.SHandler (I.EventId eventId) fn) =
            HandlerRefEnc
                { nodeId : nodeId
                , event : eventId
                , index : totalIndex
                , lindex : localIndex
                , strIndex : nodeId <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index?
                }

        nextLastHandlerIdx
            /\ (storedHandlers :: Array HandlerRefEnc)
            /\ (handlersCalls :: Array HandlerEnc)
            = foldrWithIndex
                    (\localIdx handler (totalIdx /\ storedHandlers /\ handlersCalls) ->
                        ((totalIdx + 1)
                            /\ (encodeHandlerRef (totalIdx + 1) localIdx handler : storedHandlers)
                            /\ (encodeHandler (totalIdx + 1) localIdx handler : handlersCalls)
                        )
                    ) (lastHandlerIndex /\ [] /\ [])
                    shandlers

        (children :: Array NodeEnc) /\ (innerHandlersCalls :: Array HandlerEnc) =
            foldr
                (\(child /\ itsHandlers) ( allChildren /\ allHandlers ) ->
                    (child : allChildren) /\ (itsHandlers <> allHandlers)
                )
                ([] /\ [])
                (encode' (Just $ I.NodeId nodeId) nextLastHandlerIdx <$> snodes)

        (nodeEncoded :: NodeEnc) /\ (childrenHandlers :: Array HandlerEnc) =
            NodeEnc
                { kind : I.kindToString kind
                , nodeId : nodeId
                , props : propsToMap sprops
                , children : children
                , handlers : storedHandlers
                , parent : unwrap <$> maybeParent
                }
            /\ innerHandlersCalls


-- import Data.Codec ((~))
-- import Data.Codec.Argonaut as CA

-- type Person = { name ∷ String, age ∷ Int }

-- codecPerson ∷ CA.JsonCodec Person
-- codecPerson = CA.indexedArray "Test Object" $
--   { name: _, age: _ }
--     <$> _.name ~ CA.index 0 CA.string
--     <*> _.age ~ CA.index 1 CA.int


-- import Data.Codec.Argonaut as CA
-- import Type.Proxy (Proxy(..))

-- type Person = { name ∷ String, age ∷ Int }

-- codecPerson ∷ CA.JsonCodec Person
-- codecPerson =
--   CA.object "Person" $ CA.record
--     # CA.recordProp (Proxy :: _ "name") CA.string
--     # CA.recordProp (Proxy :: _ "age") CA.int