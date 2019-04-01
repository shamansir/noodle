module Rpd.Process
    ( ProcessF(..)
    , InletInNode, OutletInNode
    , InletLabel, OutletLabel
    , TracedItem(..)
    , InletHandler(..), OutletHandler(..)
    , NodeHandlers(..), InletHandlers(..), OutletHandlers(..)
    , InletsByIndexFlow(..), OutletsByIndexFlow(..)
    , InletsByLabelFlow(..), OutletsByLabelFlow(..)
    , InletsByPathFlow(..), OutletsByPathFlow(..)
    , InletsData(..), OutletsData(..)
    , InletsMapData(..), OutletsMapData(..)
    )
    where

import Prelude

import Data.Maybe
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

import Rpd.Util (Flow, type (/->))
import Rpd.Path


type InletInNode = Int
type OutletInNode = Int


type InletLabel = String
type OutletLabel = String


data TracedItem d
    = FromInlet InletInNode d
    | FromOutlet OutletInNode d


data InletsByIndexFlow d = InletsByIndexFlow (Flow (InletInNode /\ d))
data OutletsByIndexFlow d = OutletsByIndexFlow (Flow (OutletInNode /\ d))
data InletsByLabelFlow d = InletsByLabelFlow (Flow (Maybe InletLabel /\ d))
data OutletsByLabelFlow d = OutletsByLabelFlow (Flow (Maybe (OutletLabel /\ d)))
type InletsByPathFlow d = Flow (Maybe InletPath /\ d)
type OutletsByPathFlow d = Flow (Maybe (OutletPath /\ d))


data InletsData d = InletsData (Array (Maybe d))
data OutletsData d = OutletsData (Array d)


data InletHandler d = InletHandler (d -> Effect Unit)
data OutletHandler d = OutletHandler (d -> Effect Unit)
data NodeHandlers d = NodeHandlers (Array (TracedItem d -> Effect Unit)) -- TODO: -> Rpd Unit
data InletHandlers d = Inletandlers (Array (d -> Effect Unit)) -- TODO: -> Rpd Unit
data OutletHandlers d = OutletHandlers (Array (d -> Effect Unit)) -- TODO: -> Rpd Unit


data InletsMapData key d = InletsMapData (key /-> d)
data OutletsMapData key d = OutletsMapData (key /-> d)


-- TODO: is it possible to achieve the `ProcessF function like this one?:
-- process = do
--    (Number' r) <- receive "r"
--    (Number' g) <- receive "g"
--    (Number' b) <- receive "b"
--    send "color" $ Color r g b


data ProcessF d
    = Withhold
    | PassThrough
    | ByIndex (InletsByIndexFlow d -> OutletsByIndexFlow d)
    | ByLabel (InletsByLabelFlow d -> OutletsByLabelFlow d)
    | ByPath (InletsByPathFlow d -> OutletsByPathFlow d)
     -- TODO: generalize to Foldable?
    | FoldedByIndex (InletsData d -> OutletsData d)
    | FoldedByLabel (InletsMapData String d -> OutletsMapData String d)
    -- | EffectfulByIndex (InletsData d -> Effect Unit)
    -- | EffectfulByLabel (InletsMapData String d -> Effect Unit)
