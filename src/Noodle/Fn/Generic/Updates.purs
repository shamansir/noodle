module Noodle.Fn.Generic.Updates where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Newtype (class Newtype)

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


newtype PreUpdatesRow  state inlets outlets  = PreUpdatesRow  ((Boolean /\ state) /\ (Boolean /\ InletsChange /\ inlets) /\ (Boolean /\ OutletsChange /\ outlets))
newtype PostUpdatesRow state inlets outlets  = PostUpdatesRow (ChangeFocus /\ PreUpdatesRow state inlets outlets)
newtype FocusedUpdate  state inlets outlets  = FocusedUpdate  (ChangeFocus /\ state /\ inlets /\ outlets)


derive instance Newtype (PreUpdatesRow state inlets outlets)  _
derive instance Newtype (PostUpdatesRow state inlets outlets) _
derive instance Newtype (FocusedUpdate state inlets outlets)  _