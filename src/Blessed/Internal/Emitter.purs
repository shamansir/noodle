module Blessed.Internal.Emitter where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Argonaut.Core (Json)

data CoreEvent
    = CoreEvent -- stub for the moment


class Events e where
    initial :: e
    convert :: e -> String /\ Array Json
    toCore :: e -> CoreEvent
    fromCore :: CoreEvent -> Maybe e
    -- extract :: e -> Json -> Json


instance Events CoreEvent where
    initial = CoreEvent
    convert _ = "Core" /\ []
    toCore = identity
    fromCore = Just
    -- extract _ = identity