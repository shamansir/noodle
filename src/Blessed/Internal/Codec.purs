module Blessed.Internal.Codec where

import Prelude

import Effect (Effect)
import Effect.Console as Console

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

-- TODO: Consider using encodeJson-based typeclasses


kindCodec :: CA.JsonCodec K.Subject_
kindCodec  =
    CA.prismaticCodec "Kind" K.fromString K.toString CA.string


optionRecCodec :: CA.JsonCodec I.PropJson
optionRecCodec =
    CA.object "SoleOption"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


nodeCodec :: CA.JsonCodec I.NodeEnc
nodeCodec =
    CA.fix \codec ->
        wrapIso I.NodeEnc $ CAR.object "Node"
            { nodeSubj : CA.string
            , nodeId : CA.string
            , props : CA.array optionRecCodec
            , children : CA.array codec
            , handlers : CA.array handlerRefCodec
            , parent : CAC.maybe CA.string
            }


handlerRefCodec :: CA.JsonCodec I.HandlerRefEnc
handlerRefCodec =
    wrapIso I.HandlerRefEnc $ CA.object "HandlerRef"
        (CAR.record
            { nodeId : CA.string
            , nodeSubj : CA.string
            , event : CA.string
            , index : CA.string
            , args : CA.array CA.json
            }
        )


callCommandCodec :: CA.JsonCodec I.CallCommandEnc
callCommandCodec =
    wrapIso I.CallCommandEnc $ CA.object "CallCommand"
        (CAR.record
            { type : CA.string
            , method : CA.string
            , args : CA.array CA.json
            }
        )


getCommandCodec :: CA.JsonCodec I.GetCommandEnc
getCommandCodec =
    wrapIso I.GetCommandEnc $ CA.object "GetCommand"
        (CAR.record
            { type : CA.string
            , property : CA.string
            }
        )


setCommandCodec :: CA.JsonCodec I.SetCommandEnc
setCommandCodec =
    wrapIso I.SetCommandEnc $ CA.object "SetCommand"
        (CAR.record
            { type : CA.string
            , property : CA.string
            , value : CA.json
            }
        )


processCommandCodec :: CA.JsonCodec I.ProcessCommandEnc
processCommandCodec =
    wrapIso I.ProcessCommandEnc $ CA.object "ProcessCommand"
        (CAR.record
            { type : CA.string
            , method : CA.string
            , args : CA.array CA.json
            }
        )

commandDumpCodec :: CA.JsonCodec I.CallDump
commandDumpCodec =
    wrapIso I.CallDump $ CA.object "CallDump"
        (CAR.record
            { nodeId : CA.string
            , nodeSubj : CA.string
            , event : CA.string
            , args : CA.array CA.json
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


encode' :: Maybe NK.RawNodeKey -> I.SNode -> I.NodeEnc /\ Array I.HandlerCallEnc
encode'
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

        encodeHandler :: Int -> I.SHandler -> I.HandlerCallEnc
        encodeHandler localIndex (I.SHandler (I.EventId eventId) args fn) =
            I.HandlerCallEnc
                { nodeId : rawNodeKey.id
                , nodeSubj : K.toString rawNodeKey.subject
                , event : eventId
                , args
                , index : rawNodeKey.id <> "-" <> eventId <> "-" <> show localIndex -- include parent id & total index
                , call : fn (I.newRegistry) (NK.RawNodeKey rawNodeKey)
                }

        encodeHandlerRef :: Int -> I.SHandler -> I.HandlerRefEnc
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
                (encode' (Just $ NK.RawNodeKey rawNodeKey) <$> snodes)

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
            CA.encode callCommandCodec $ I.CallCommandEnc $ { args, method : cmd, type : "call" }
        C.Get { prop } ->
            CA.encode getCommandCodec $ I.GetCommandEnc $ { property : prop, type : "get" }
        C.Set { prop, value } ->
            CA.encode setCommandCodec $ I.SetCommandEnc $ { value, property : prop, type : "set" }
        C.WithProcess { cmd, args } ->
            CA.encode processCommandCodec $ I.ProcessCommandEnc $ { args, method : cmd, type : "process" }


encodeCommand :: C.Command -> I.CommandEnc
encodeCommand =
    I.CommandEnc <<< commandToJson


encodeDump :: I.CallDump -> Json
encodeDump = CA.encode commandDumpCodec