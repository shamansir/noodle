module Blessed.Internal.Codec where

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

-- TODO: Consider using encodeJson-based typeclasses


subject_ :: CA.JsonCodec K.Subject_
subject_  =
    CA.prismaticCodec "Kind" K.fromString K.toString CA.string


propJson :: CA.JsonCodec I.PropJson
propJson =
    CA.object "SoleOption"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


nodeEnc :: CA.JsonCodec I.NodeEnc
nodeEnc =
    CA.fix \codec ->
        wrapIso I.NodeEnc $ CAR.object "Node"
            { nodeSubj : CA.string
            , nodeId : CA.string
            , props : CA.array propJson
            , children : CA.array codec
            , handlers : CA.array handlerRefEnc
            , parent : CAC.maybe CA.string
            }


handlerRefEnc :: CA.JsonCodec I.HandlerRefEnc
handlerRefEnc =
    wrapIso I.HandlerRefEnc $ CA.object "HandlerRef"
        (CAR.record
            { nodeId : CA.string
            , nodeSubj : CA.string
            , event : CA.string
            , index : CA.string
            }
        )


callCommandEnc :: CA.JsonCodec I.CallCommandEnc
callCommandEnc =
    wrapIso I.CallCommandEnc $ CA.object "CallCommand"
        (CAR.record
            { type : CA.string
            , method : CA.string
            , args : CA.array CA.json
            }
        )


getCommandEnc :: CA.JsonCodec I.GetCommandEnc
getCommandEnc =
    wrapIso I.GetCommandEnc $ CA.object "GetCommand"
        (CAR.record
            { type : CA.string
            , property : CA.string
            }
        )


setCommandEnc :: CA.JsonCodec I.SetCommandEnc
setCommandEnc =
    wrapIso I.SetCommandEnc $ CA.object "SetCommand"
        (CAR.record
            { type : CA.string
            , property : CA.string
            , value : CA.json
            }
        )


processCommandEnc :: CA.JsonCodec I.ProcessCommandEnc
processCommandEnc =
    wrapIso I.ProcessCommandEnc $ CA.object "ProcessCommand"
        (CAR.record
            { type : CA.string
            , method : CA.string
            , args : CA.array CA.json
            }
        )

callDump :: CA.JsonCodec I.CallDump
callDump =
    wrapIso I.CallDump $ CA.object "CallDump"
        (CAR.record
            { nodeId : CA.string
            , nodeSubj : CA.string
            , event : CA.string
            , args : CA.array CA.json
            }
        )