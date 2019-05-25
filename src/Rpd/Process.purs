module Rpd.Process
    ( ProcessF(..)
    -- , InletInNode, OutletInNode
    -- , InletLabel, OutletLabel
    , InletAlias, OutletAlias
    , TracedItem(..)
    , InletHandler(..), OutletHandler(..)
    , NodeHandlers(..), InletHandlers(..), OutletHandlers(..)
    -- , InletsByIndexFlow(..), OutletsByIndexFlow(..)
    -- , InletsByLabelFlow(..), OutletsByLabelFlow(..)
    -- , InletsByPathFlow(..), OutletsByPathFlow(..)
    , InletsData(..), OutletsData(..)
    -- , InletsMapData(..), OutletsMapData(..)
    )
    where

import Prelude

import Data.Maybe
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

import Rpd.Util (Flow, type (/->))
import Rpd.Path


-- type InletInNode = Int
-- type OutletInNode = Int


-- type InletLabel = Alias
-- type OutletLabel = Alias


type InletAlias = Alias
type OutletAlias = Alias


data TracedItem d
    = FromInlet Alias d
    | FromOutlet Alias d


-- data InletsByIndexFlow d = InletsByIndexFlow (Flow (InletInNode /\ d))
-- data OutletsByIndexFlow d = OutletsByIndexFlow (Flow (OutletInNode /\ d))
-- data InletsByLabelFlow d = InletsByLabelFlow (Flow (Maybe InletLabel /\ d))
-- data OutletsByLabelFlow d = OutletsByLabelFlow (Flow (Maybe (OutletLabel /\ d)))
-- type InletsByPathFlow d = Flow (Maybe InletPath /\ d)
-- type OutletsByPathFlow d = Flow (Maybe (OutletPath /\ d))


data InletsData d = InletsData (Array d)
data OutletsData d = OutletsData (Array d)


-- data InletsMapData key d = InletsMapData (key /-> d)
-- data OutletsMapData key d = OutletsMapData (key /-> d)


data InletHandler d = InletHandler (d -> Effect Unit)
data OutletHandler d = OutletHandler (d -> Effect Unit)
data NodeHandlers d = NodeHandlers (Array (TracedItem d -> Effect Unit)) -- TODO: -> Rpd Unit
data InletHandlers d = Inletandlers (Array (d -> Effect Unit)) -- TODO: -> Rpd Unit
data OutletHandlers d = OutletHandlers (Array (d -> Effect Unit)) -- TODO: -> Rpd Unit


-- TODO: is it possible to achieve the `ProcessF function like this one?:
-- process = do
--    (Number' r) <- receive "r"
--    (Number' g) <- receive "g"
--    (Number' b) <- receive "b"
--    send "color" $ Color r g b


-- TODO: use IAlias -> data / OAlias -> data functions instead, see TODO.md for more info


-- type Receive d = InletAlias -> Maybe d
-- type Send d = OutletAlias -> Maybe d


data ProcessF d
    = Withhold
    -- | PassThrough -- TODO
    -- | Process (Receive d -> Effect (Send d))
    -- TODO: one more option to produce Aff (and then cancel it on next iteration)
    | Process ((InletAlias -> Maybe d) -> Effect (OutletAlias -> Maybe d))
