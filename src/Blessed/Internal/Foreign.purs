module Blessed.Internal.Foreign where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref


import Data.Array ((:))
import Data.Array as Array
import Data.Tuple (snd) as Tuple
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

import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.JsApi as I
import Blessed.Internal.Command as C
import Blessed.Internal.Codec as Codec


encode :: forall state. Ref state -> I.SNode state -> I.BlessedEnc
encode stateRef rootNode =
    case encodeRoot stateRef rootNode of
        nodeEnc /\ handlers ->
            I.BlessedEnc $
                { root : CA.encode Codec.nodeEnc nodeEnc
                , handlersFns : handlers
                }


encodeRoot :: forall state. Ref state -> I.SNode state -> I.NodeEnc /\ Array I.HandlerCallEnc
encodeRoot stateRef = encode' stateRef Nothing


encode' :: forall state. Ref state -> Maybe NK.RawNodeKey -> I.SNode state -> I.NodeEnc /\ Array I.HandlerCallEnc
encode'
    stateRef
    maybeParent
    (I.SNode (NK.RawNodeKey rawNodeKey) sprops snodes shandlers)

    =
    -- BlessedEnc (CA.encode CA.null unit /\ [ HandlerEnc { nodeId : "test", event: "test", index : -1, call: const $ Console.log "foo" }])
    nodeEncoded /\ (handlersCalls <> childrenHandlers)
    where

        -- propsToMap :: Array I.SProp -> Map String Json
        -- propsToMap = Map.fromFoldable <<< map I.unwrapProp

        adaptProps :: Array I.SProp -> Array I.PropJson
        adaptProps = map (I.unwrapProp >>> \(name /\ value ) -> { name, value })

        encodeHandler :: Int -> I.SHandler state -> I.HandlerCallEnc
        encodeHandler localIndex (I.SHandler (I.EventId eventId) args fn) =
            I.HandlerCallEnc
                { nodeId : rawNodeKey.id
                , nodeSubj : K.toString rawNodeKey.subject
                , event : eventId
                , args
                , index : rawNodeKey.id <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index
                , call : fn stateRef $ NK.RawNodeKey rawNodeKey
                }

        encodeHandlerRef :: Int -> I.SHandler state -> I.HandlerRefEnc
        encodeHandlerRef localIndex (I.SHandler (I.EventId eventId) args fn) =
            I.HandlerRefEnc
                { nodeId : rawNodeKey.id
                , nodeSubj : K.toString rawNodeKey.subject
                , event : eventId
                , args
                , index : rawNodeKey.id <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index?
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
                (encode' stateRef (Just $ NK.RawNodeKey rawNodeKey) <$> snodes)

        (nodeEncoded :: I.NodeEnc) /\ (childrenHandlers :: Array I.HandlerCallEnc) =
            I.NodeEnc
                { nodeId : rawNodeKey.id
                , nodeSubj : K.toString rawNodeKey.subject
                , props : adaptProps sprops
                , children : children
                , handlers : storedHandlers
                , parent : _.id <$> unwrap <$> maybeParent
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


commandToJson :: C.Command -> Json
commandToJson =
    case _ of
        C.Call { cmd, args } ->
            CA.encode Codec.callCommandEnc $ I.CallCommandEnc $ { args, method : cmd, type : "call" }
        C.Get { prop } ->
            CA.encode Codec.getCommandEnc $ I.GetCommandEnc $ { property : prop, type : "get" }
        C.Set { prop, value } ->
            CA.encode Codec.setCommandEnc $ I.SetCommandEnc $ { value, property : prop, type : "set" }
        C.WithProcess { cmd, args } ->
            CA.encode Codec.processCommandEnc $ I.ProcessCommandEnc $ { args, method : cmd, type : "process" }


encodeCommand :: C.Command -> I.CommandEnc
encodeCommand =
    I.CommandEnc <<< commandToJson


encodeDump :: I.CallDump -> Json
encodeDump = CA.encode Codec.callDump