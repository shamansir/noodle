module Noodle.Fn.Generic.Updates where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

import Noodle.Id (InletR, OutletR)


data InletsChange
    = SingleInlet InletR
    | AllInlets


data OutletsChange
    = SingleOutlet OutletR
    | AllOutlets


data ChangeFocus
    = Everything
    | StateChange
    | AllInletsChange
    | InletChange InletR
    | AllOutletsChange
    | OutletChange OutletR


instance Show ChangeFocus where
    show :: ChangeFocus -> String
    show =
        case _ of
            Everything -> "all"
            StateChange -> "state"
            AllInletsChange -> "inlets"
            InletChange inletR -> "inlet " <> show inletR
            AllOutletsChange -> "outlets"
            OutletChange outletR -> "outlet " <> show outletR


inletChangeToMaybe :: InletsChange -> Maybe InletR
inletChangeToMaybe (SingleInlet iid) = Just iid
inletChangeToMaybe AllInlets = Nothing


outletChangeToMaybe :: OutletsChange -> Maybe OutletR
outletChangeToMaybe (SingleOutlet oid) = Just oid
outletChangeToMaybe AllOutlets = Nothing


type PreUpdatesRow state inlets outlets = (Boolean /\ state) /\ (Boolean /\ InletsChange /\ inlets) /\ (Boolean /\ OutletsChange /\ outlets)
type PostUpdatesRow state inlets outlets = ChangeFocus /\ PreUpdatesRow state inlets outlets
type FocusedUpdate state inlets outlets = ChangeFocus /\ state /\ inlets /\ outlets