module Blessed.Internal.Dump where



import Prelude

import Effect.Class (liftEffect, class MonadEffect)
import Effect.Aff (launchAff_)

import Data.Bifunctor (bimap)
import Data.Profunctor (wrapIso)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Array as Array

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (stringify) as Json
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile)
import Node.Path (FilePath)

import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.NodeKey as I
import Blessed.Internal.Emitter as I
import Blessed.Internal.Command as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.Foreign as Foreign



commandsDumpPath :: FilePath
commandsDumpPath = "./commands_dump.txt"


newtype CallDump =
    CallDump
        { marker :: String
        , nodeId :: String
        , nodeSubj :: String
        , event :: String
        , args :: Array Json
        }


derive instance Newtype CallDump _


commandToPerform :: forall m. MonadEffect m => I.Command -> m Unit
commandToPerform =
    commandWasPerformed


commandWasPerformed :: forall m. MonadEffect m => I.Command -> m Unit
commandWasPerformed =
    liftEffect
        <<< launchAff_
        <<< appendTextFile UTF8 commandsDumpPath
        <<< cmdTupleToLine
        <<< bimap Json.stringify (Array.length >>> show)
        <<< Foreign.commandToJson
    where
        cmdTupleToLine (callDump /\ handlersCountStr) = callDump <> " (" <> handlersCountStr <> ")" <> "\n"


handlerCall :: forall m. MonadEffect m => I.RawNodeKey -> I.EventId -> Array Json -> m Unit
handlerCall (I.RawNodeKey nodeKey) (I.EventId event) args =
    liftEffect
        $ launchAff_
        $ appendTextFile UTF8 commandsDumpPath
        $ (<>) "\n"
        $ Json.stringify
        $ encode
        $ CallDump
            { marker : "CallDump"
            , args, event, nodeId : nodeKey.id
            , nodeSubj : K.toString nodeKey.subject
            }


encode :: CallDump -> Json
encode = CA.encode codec -- TODO: prettify


codec :: CA.JsonCodec CallDump
codec =
    wrapIso CallDump $ CA.object "CallDump"
        (CAR.record
            { marker : CA.string
            , nodeId : CA.string
            , nodeSubj : CA.string
            , event : CA.string
            , args : CA.array CA.json
            }
        )