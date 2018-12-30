module Rpd.Process
    ( ProcessF(..)
    , InletsFlow(..), OutletsFlow(..)
    , InletsByIndexFlow(..), OutletsByIndexFlow(..)
    , InletsByLabelFlow(..), OutletsByLabelFlow(..)
    , InletsByPathFlow(..), OutletsByPathFlow(..)
    )
    where

import Data.Tuple.Nested (type (/\))

import Rpd.Util (Flow)
import Rpd.Path


data InletsByIndexFlow d = InletsByIndexFlow (Flow (Int /\ d))
data OutletsByIndexFlow d = OutletsByIndexFlow (Flow (Int /\ d))
data InletsByLabelFlow d = InletsByLabelFlow (Flow (String /\ d))
data OutletsByLabelFlow d = OutletsByLabelFlow (Flow (String /\ d))
type InletsByPathFlow d = Flow (InletPath /\ d)
type OutletsByPathFlow d = Flow (OutletPath /\ d)


data InletsFlow d = InletsFlow (Flow (Int /\ d))
data OutletsFlow d = OutletsFlow (Flow (Int /\ d))


data ProcessF d
    = Withhold
    | ByIndex (InletsByIndexFlow d -> OutletsByIndexFlow d)
    | ByLabel (InletsByLabelFlow d -> OutletsByLabelFlow d)
    | ByPath (InletsByPathFlow d -> OutletsByPathFlow d)
