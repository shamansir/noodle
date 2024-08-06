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
            InletChange inputR -> "input " <> show inputR
            AllOutletsChange -> "outlets"
            OutletChange outputR -> "output " <> show outputR


inputChangeToMaybe :: InletsChange -> Maybe InletR
inputChangeToMaybe (SingleInlet iid) = Just iid
inputChangeToMaybe AllInlets = Nothing


outputChangeToMaybe :: OutletsChange -> Maybe OutletR
outputChangeToMaybe (SingleOutlet oid) = Just oid
outputChangeToMaybe AllOutlets = Nothing


type PreUpdatesRow state inlets outlets = (Boolean /\ state) /\ (Boolean /\ InletsChange /\ inlets) /\ (Boolean /\ OutletsChange /\ outlets)
type PostUpdatesRow state inlets outlets = ChangeFocus /\ PreUpdatesRow state inlets outlets
type FocusedUpdate state inlets outlets = ChangeFocus /\ state /\ inlets /\ outlets