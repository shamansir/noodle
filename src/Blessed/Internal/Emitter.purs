module Blessed.Internal.Emitter where

import Prelude

import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Argonaut.Core (Json)

import Blessed.Internal.BlessedSubj (Subject)

data CoreEvent
    = CoreEvent String (Array Json)


class Events e where
    initial :: e
    convert :: e -> String /\ Array Json


instance Events CoreEvent where
    initial = CoreEvent "Core" []
    convert (CoreEvent name args) = name /\ args


class Events e <= Fires (subj :: Subject) e


toCore :: forall e. Events e => e -> CoreEvent
toCore = convert >>> uncurry CoreEvent
