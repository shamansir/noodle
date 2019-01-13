module Rpd.Process
    ( ProcessF(..)
    , InletsFlow(..), OutletsFlow(..)
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


-- TODO:

-- data FlowMsg = Bang | Skip | Pass v | Decline v | Error x ...

data Incoming x d
    = Identified x d
    | Unidentified d


data Outgoing x d
    = Send x d
    | Skip


data InletsByIndexFlow d = InletsByIndexFlow (Flow (Int /\ d))
data OutletsByIndexFlow d = OutletsByIndexFlow (Flow (Int /\ d))
data InletsByLabelFlow d = InletsByLabelFlow (Flow (Maybe String /\ d))
data OutletsByLabelFlow d = OutletsByLabelFlow (Flow (Maybe (String /\ d)))
type InletsByPathFlow d = Flow (Maybe InletPath /\ d)
type OutletsByPathFlow d = Flow (Maybe (OutletPath /\ d))


data InletsData d = InletsData (Array (Maybe d))
data OutletsData d = OutletsData (Array d)


data InletsFlow d = InletsFlow (Flow (Int /\ d))
data OutletsFlow d = OutletsFlow (Flow (Maybe (Int /\ d)))


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
