module Blessed.Internal.Codec where

import Prelude

import Effect (Effect)
import Effect.Console as Console

import Data.Array ((:))
import Data.Array as Array
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


kindCodec :: CA.JsonCodec I.Kind
kindCodec  =
    CA.prismaticCodec "Kind" I.kindFromString I.kindToString CA.string


propertyRecCodec :: CA.JsonCodec I.PropJson
propertyRecCodec =
    CA.object "OnlyProperty"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


nodeCodec :: CA.JsonCodec I.NodeEnc
nodeCodec =
    CA.fix \codec ->
        wrapIso I.NodeEnc $ CAR.object "Node"
            { kind : CA.string
            , nodeId : CA.string
            , props : CA.array propertyRecCodec
            , children : CA.array codec
            , handlers : CA.array handlerRefCodec
            , parent : CAC.maybe CA.string
            }


handlerRefCodec :: CA.JsonCodec I.HandlerRefEnc
handlerRefCodec =
    wrapIso I.HandlerRefEnc $ CA.object "HandlerRef"
        (CAR.record
            { nodeId : CA.string
            , event : CA.string
            , index : CA.string
            }
        )

encode :: I.SNode -> I.BlessedEnc
encode rootNode =
    case encodeRoot rootNode of
        nodeEnc /\ handlers ->
            I.BlessedEnc $
                { root : CA.encode nodeCodec nodeEnc
                , handlersFns : handlers
                }


encodeRoot :: I.SNode -> I.NodeEnc /\ Array I.HandlerCallEnc
encodeRoot = encode' Nothing


encode' :: Maybe I.NodeId -> I.SNode -> I.NodeEnc /\ Array I.HandlerCallEnc
encode'
    maybeParent
    (I.SNode kind (I.NodeId nodeId) sprops snodes shandlers)

    =
    -- BlessedEnc (CA.encode CA.null unit /\ [ HandlerEnc { nodeId : "test", event: "test", index : -1, call: const $ Console.log "foo" }])
    nodeEncoded /\ (handlersCalls <> childrenHandlers)
    where

        -- propsToMap :: Array I.SProp -> Map String Json
        -- propsToMap = Map.fromFoldable <<< map I.unwrapProp

        adaptProps :: Array I.SProp -> Array I.PropJson
        adaptProps = map (I.unwrapProp >>> \(name /\ value ) -> { name, value })

        encodeHandler :: Int -> I.SHandler -> I.HandlerCallEnc
        encodeHandler localIndex (I.SHandler (I.EventId eventId) fn) =
            I.HandlerCallEnc
                { nodeId : nodeId
                , event : eventId
                , index : nodeId <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index
                , call : fn (I.newRegistry) (I.NodeId nodeId)
                }

        encodeHandlerRef :: Int -> I.SHandler -> I.HandlerRefEnc
        encodeHandlerRef localIndex (I.SHandler (I.EventId eventId) fn) =
            I.HandlerRefEnc
                { nodeId : nodeId
                , event : eventId
                , index : nodeId <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index?
                }

        (storedHandlers :: Array I.HandlerRefEnc) /\ (handlersCalls :: Array I.HandlerCallEnc)
            = foldrWithIndex
                    (\localIdx handler (storedHandlers /\ handlersCalls) ->
                        (
                            (encodeHandlerRef localIdx handler : storedHandlers)
                            /\ (encodeHandler localIdx handler : handlersCalls)
                        )
                    )
                    ([] /\ [])
                    shandlers

        (children :: Array I.NodeEnc) /\ (innerHandlersCalls :: Array I.HandlerCallEnc) =
            foldr
                (\(child /\ itsHandlers) ( allChildren /\ allHandlers ) ->
                    (child : allChildren) /\ (itsHandlers <> allHandlers)
                )
                ([] /\ [])
                (encode' (Just $ I.NodeId nodeId) <$> snodes)

        (nodeEncoded :: I.NodeEnc) /\ (childrenHandlers :: Array I.HandlerCallEnc) =
            I.NodeEnc
                { kind : I.kindToString kind
                , nodeId : nodeId
                , props : adaptProps sprops
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