module Noodle.Fn.Generic.Updates where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (class Newtype)

import Noodle.Id (InletR, OutletR)


data InletsUpdate
    = SingleInlet InletR
    | AllInlets


data OutletsUpdate
    = SingleOutlet OutletR
    | AllOutlets


data UpdateFocus
    = Everything
    | StateUpdate
    | AllInletsUpdate
    | InletUpdate InletR
    | AllOutletsUpdate
    | OutletUpdate OutletR


data Update state inlets outlets
    = UpdateEverything state inlets outlets
    | UpdateState state
    | UpdateInlets InletsUpdate inlets
    | UpdateOutlets OutletsUpdate outlets


type MergedUpdateRec state inlets outlets =
    { focus :: UpdateFocus
    , state :: state
    , inlets :: inlets
    , outlets :: outlets
    }


newtype MergedUpdate state inlets outlets
    = MergedUpdate (MergedUpdateRec state inlets outlets)


instance Show UpdateFocus where
    show :: UpdateFocus -> String
    show =
        case _ of
            Everything -> "all"
            StateUpdate -> "state"
            AllInletsUpdate -> "inlets"
            InletUpdate inletR -> "inlet " <> show inletR
            AllOutletsUpdate -> "outlets"
            OutletUpdate outletR -> "outlet " <> show outletR


instance (Show state, Show inlets, Show outlets) => Show (Update state inlets outlets) where
    show = case _ of
        UpdateEverything state inlets outlets ->
            "all : " <> show state <> " :: " <> show inlets <> " :: " <> show outlets
        UpdateState state ->
            "state : " <> show state
        UpdateInlets update inlets ->
            case update of
                AllInlets -> "inlets"
                SingleInlet inlet -> "in@" <> show inlet
            <> ": " <> show inlets
        UpdateOutlets update outlets ->
            case update of
                AllOutlets -> "outlets"
                SingleOutlet outlet -> "out@" <> show outlet
            <> ": " <> show outlets


instance (Show state, Show inlets, Show outlets) => Show (MergedUpdate state inlets outlets) where
    show = case _ of
        MergedUpdate { focus, state, inlets, outlets } ->
            show focus <> " : " <> show state <> " :: " <> show inlets <> " :: " <> show outlets


inletUpdateToMaybe :: InletsUpdate -> Maybe InletR
inletUpdateToMaybe (SingleInlet iid) = Just iid
inletUpdateToMaybe AllInlets = Nothing


outletUpdateToMaybe :: OutletsUpdate -> Maybe OutletR
outletUpdateToMaybe (SingleOutlet oid) = Just oid
outletUpdateToMaybe AllOutlets = Nothing


startCollecting :: forall state inlets outlets. state -> inlets -> outlets -> MergedUpdate state inlets outlets
startCollecting state inlets outlets = MergedUpdate { focus : Everything, state, inlets, outlets }


fold :: forall state inlets outlets. Update state inlets outlets -> MergedUpdate state inlets outlets -> MergedUpdate state inlets outlets
fold lastUpdate (MergedUpdate { state, inlets, outlets }) =
    MergedUpdate $ case lastUpdate of
        UpdateEverything nextState nextInlets nextOutlets ->
            { focus : Everything, state : nextState, inlets : nextInlets, outlets : nextOutlets }
        UpdateState nextState ->
            { focus : StateUpdate, state : nextState, inlets, outlets }
        UpdateInlets inletUpdate nextInlets ->
            { focus : case inletUpdate of
                SingleInlet inletR -> InletUpdate inletR
                AllInlets -> AllInletsUpdate
            , state, inlets : nextInlets, outlets }
        UpdateOutlets outletUpdate nextOutlets ->
            { focus : case outletUpdate of
                SingleOutlet inletR -> OutletUpdate inletR
                AllOutlets -> AllOutletsUpdate
            , state, inlets, outlets : nextOutlets }


{-
toTuple :: forall state inlets outlets. MergedUpdate state inlets outlets -> UpdateFocus /\ state /\ inlets /\ outlets
toTuple (MergedUpdate { focus, state, inlets, outlets }) = focus /\ state /\ inlets /\ outlets
-}


toRecord :: forall state inlets outlets. MergedUpdate state inlets outlets -> MergedUpdateRec state inlets outlets
toRecord (MergedUpdate rec) = rec -- unwrap


mapState :: forall state state' inlets outlets. (state -> state') -> Update state inlets outlets -> Update state' inlets outlets
mapState f = case _ of
    UpdateEverything state inlets outlets -> UpdateEverything (f state) inlets outlets
    UpdateState state -> UpdateState $ f state
    UpdateInlets upd inlets -> UpdateInlets upd inlets
    UpdateOutlets upd outlets -> UpdateOutlets upd outlets


mapInlets :: forall state inlets inlets' outlets. (inlets -> inlets') -> Update state inlets outlets -> Update state inlets' outlets
mapInlets f = case _ of
    UpdateEverything state inlets outlets -> UpdateEverything state (f inlets) outlets
    UpdateState state -> UpdateState state
    UpdateInlets upd inlets -> UpdateInlets upd $ f inlets
    UpdateOutlets upd outlets -> UpdateOutlets upd outlets


mapOutlets :: forall state inlets outlets outlets'. (outlets -> outlets') -> Update state inlets outlets -> Update state inlets outlets'
mapOutlets f = case _ of
    UpdateEverything state inlets outlets -> UpdateEverything state inlets $ f outlets
    UpdateState state -> UpdateState state
    UpdateInlets upd inlets -> UpdateInlets upd inlets
    UpdateOutlets upd outlets -> UpdateOutlets upd $ f outlets


mergedMapState :: forall state state' inlets outlets. (state -> state') -> MergedUpdate state inlets outlets -> MergedUpdate state' inlets outlets
mergedMapState f (MergedUpdate { focus, state, inlets, outlets }) =
    MergedUpdate { focus, state : f state, inlets, outlets }


mergedMapInlets :: forall state inlets inlets' outlets. (inlets -> inlets') -> MergedUpdate state inlets outlets -> MergedUpdate state inlets' outlets
mergedMapInlets f (MergedUpdate { focus, state, inlets, outlets }) =
    MergedUpdate { focus, state, inlets: f inlets, outlets }


mergedMapOutlets :: forall state inlets outlets outlets'. (outlets -> outlets') -> MergedUpdate state inlets outlets -> MergedUpdate state inlets outlets'
mergedMapOutlets f (MergedUpdate { focus, state, inlets, outlets }) =
    MergedUpdate { focus, state, inlets, outlets : f outlets }
