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

data CoreEvent ie
    = CoreEvent (Maybe ie) String (Array Json)


class Events e where
    initial :: e
    convert :: e -> String /\ Array Json
    uniqueId :: e -> String
    -- response :: e -> (forall a. Json -> Maybe a)


instance Events ie => Events (CoreEvent ie) where
    initial = CoreEvent Nothing "Core" []
    convert (CoreEvent _ name args) = name /\ args
    uniqueId (CoreEvent maybeIE name _) = maybe name uniqueId maybeIE


class Events e <= Fires (subj :: Subject) e


toCore :: forall e. Events e => e -> CoreEvent e
toCore ie = uncurry (CoreEvent $ Just ie) $ convert ie


defaultUniqueId :: forall e. Events e => e -> String
defaultUniqueId = convert >>> Tuple.fst


newtype EventId = EventId { "type" :: String, uniqueId :: String }


typeOf :: EventId -> String
typeOf = unwrap >>> _.type


derive instance Newtype EventId _
derive newtype instance EncodeJson EventId


-- instance EncodeJson EventId where
--     encodeJson (EventId { uniqueId }) = encodeJson uniqueId -- FIXME: temporarily


toEventId :: forall e. Events e => e -> EventId
toEventId e = EventId { type : Tuple.fst $ convert e, uniqueId : uniqueId e }


split :: forall e. Events e => e -> EventId /\ Array Json
split e = toEventId e /\ Tuple.snd (convert e)