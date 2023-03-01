module Blessed.Internal.Emitter where

import Prelude

import Data.Tuple (uncurry)
import Data.Tuple (fst, snd) as Tuple
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.BlessedSubj (Subject)

data CoreEvent
    = CoreEvent EventId (Array Json)


class Events e where
    initial :: e
    convert :: e -> String /\ Array Json
    uniqueId :: e -> String
    -- response :: e -> (forall a. Json -> Maybe a)


-- instance Events ie => Events (CoreEvent ie) where
instance Events CoreEvent where
    initial = initialCore
    convert (CoreEvent (EventId eventId) args) = eventId.type /\ args
    uniqueId (CoreEvent (EventId { uniqueId }) _) = uniqueId


class Events e <= Fires (subj :: Subject) e


toCore :: forall e. Events e => e -> CoreEvent
toCore = uncurry CoreEvent <<< split
    -- case split ie of
    --     eventId /\ args ->
    --         CoreEvent eventId args
        -- else uncurry (CoreEvent $ Just ie) $ convert ie


defaultUniqueId :: forall e. Events e => e -> String
defaultUniqueId = convert >>> Tuple.fst


initialCore :: CoreEvent
initialCore =
    CoreEvent
        (EventId { initial : true, type : "init", uniqueId : "core-init" })
        []


newtype EventId = EventId { initial :: Boolean, "type" :: String, uniqueId :: String }


typeOf :: EventId -> String
typeOf = unwrap >>> _.type


derive instance Newtype EventId _
derive newtype instance EncodeJson EventId


-- instance EncodeJson EventId where
--     encodeJson (EventId { uniqueId }) = encodeJson uniqueId -- FIXME: temporarily


toEventId :: forall e. Events e => e -> EventId
toEventId e =
    EventId
        { initial : uniqueId (initial :: e) == uniqueId e
        , type : Tuple.fst $ convert e
        , uniqueId : uniqueId e
        }


split :: forall e. Events e => e -> EventId /\ Array Json
split e = toEventId e /\ Tuple.snd (convert e)