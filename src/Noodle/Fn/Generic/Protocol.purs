module Noodle.Fn.Generic.Protocol where

import Prelude

import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Signal as Signal
import Signal.Channel (channel)
import Signal.Channel (subscribe, send) as Channel

import Noodle.Id (InletR, OutletR)

import Noodle.Fn.Generic.Tracker (Tracker)
import Noodle.Fn.Generic.Updates as U


type Protocol state inlets outlets =
    { getInlets :: Unit -> Effect (U.InletsChange /\ inlets)
    , getOutlets :: Unit -> Effect (U.OutletsChange /\ outlets)
    , getState :: Unit -> Effect state
    , modifyInlets :: (inlets -> U.InletsChange /\ inlets) -> Effect Unit
    , modifyOutlets :: (outlets -> U.OutletsChange /\ outlets) -> Effect Unit
    , modifyState :: (state -> state) -> Effect Unit
    }


make
    :: forall state inlets outlets m
    .  MonadEffect m
    => state
    -> inlets
    -> outlets
    -> m (Tracker state inlets outlets /\ Protocol state inlets outlets)
make state inlets outlets =
    liftEffect $ do

         -- boolean flags help to find out which signal was updated the latest in the merged all changes by flipping them on modification

        let
            stateInit   = true /\ state
            inletsInit  = true /\ U.AllInlets /\ inlets
            outletsInit = true /\ U.AllOutlets /\ outlets

            (initAll :: U.PostUpdatesRow state inlets outlets) = U.PostUpdatesRow $ U.Everything /\ (U.PreUpdatesRow $ stateInit /\ inletsInit /\ outletsInit)

        stateCh   <- channel stateInit
        inletsCh  <- channel inletsInit
        outletsCh <- channel outletsInit
        -- allChangesCh <- channel initAll

        let
            stateSig   = Channel.subscribe stateCh
            inletsSig  = Channel.subscribe inletsCh
            outletsSig = Channel.subscribe outletsCh
            changesSig = Signal.foldp foldUpdates initAll (toPreUpdateRow <$> stateSig <*> inletsSig <*> outletsSig)

            foldUpdates :: U.PreUpdatesRow state inlets outlets -> U.PostUpdatesRow state inlets outlets -> U.PostUpdatesRow state inlets outlets
            foldUpdates
                lastChange@(U.PreUpdatesRow ((bState /\ state) /\ (bInlet /\ inletChange /\ is) /\ (bOutlet /\ outletChange /\ os)))
                (U.PostUpdatesRow (_ /\ U.PreUpdatesRow ((bStatePrev /\ _) /\ (bInletPrev /\ _ /\ _) /\ (bOutletPrev /\ _))))
                =
                    U.PostUpdatesRow $
                    if bState == not bStatePrev then
                        U.StateChange /\ lastChange
                    else if bInlet == not bInletPrev then
                        case inletChange of
                            U.AllInlets ->
                                U.AllInletsChange /\ lastChange
                            U.SingleInlet inletR ->
                                U.InletChange inletR /\ lastChange
                    else if bOutlet == not bOutletPrev then
                        case outletChange of
                            U.AllOutlets ->
                                U.AllOutletsChange /\ lastChange
                            U.SingleOutlet outletR ->
                                U.OutletChange outletR /\ lastChange
                    else U.Everything /\ lastChange


            toPreUpdateRow
                :: (Boolean /\ state)
                -> (Boolean /\ U.InletsChange /\ inlets)
                -> (Boolean /\ U.OutletsChange /\ outlets)
                -> U.PreUpdatesRow state inlets outlets
            toPreUpdateRow s is os = U.PreUpdatesRow $ s /\ is /\ os

            extractChanges
                :: U.PostUpdatesRow state inlets outlets
                -> U.FocusedUpdate state inlets outlets
            extractChanges
                = unwrap >>> map (unwrap >>> bimap Tuple.snd (bimap (Tuple.snd >>> Tuple.snd) (Tuple.snd >>> Tuple.snd))) >>> U.FocusedUpdate

        let
            tracker :: Tracker state inlets outlets
            tracker =
                { state   : Tuple.snd <$> stateSig
                , inlets  : Tuple.snd <$> inletsSig
                , outlets : Tuple.snd <$> outletsSig
                , all     : unwrap <$> extractChanges <$> changesSig
                }

            protocol :: Protocol state inlets outlets
            protocol =
                { getInlets  : const $ liftEffect $ Signal.get $ Tuple.snd <$> inletsSig
                , getOutlets : const $ liftEffect $ Signal.get $ Tuple.snd <$> outletsSig
                , getState   : const $ liftEffect $ Signal.get $ Tuple.snd <$> stateSig
                -- below we flip the flags on every update in particular signal
                , modifyInlets  : \f -> liftEffect $ Signal.get inletsSig  >>= bimap not (Tuple.snd >>> f) >>> Channel.send inletsCh
                , modifyOutlets : \f -> liftEffect $ Signal.get outletsSig >>= bimap not (Tuple.snd >>> f) >>> Channel.send outletsCh
                , modifyState   : \f -> liftEffect $ Signal.get stateSig   >>= bimap not f >>> Channel.send stateCh
                }

        pure $ tracker /\ protocol


_modifyInlet :: forall state inlets outlets. (inlets -> inlets) -> InletR -> Protocol state inlets outlets -> Effect Unit
_modifyInlet f inlet protocol =
    protocol.modifyInlets
        (\curInlets ->
            (U.SingleInlet $ inlet) /\ f curInlets
        )


_modifyOutlet :: forall state inlets outlets. (outlets -> outlets) -> OutletR -> Protocol state inlets outlets -> Effect Unit
_modifyOutlet f outlet protocol =
    protocol.modifyOutlets
        (\curOutlets ->
            (U.SingleOutlet outlet) /\ f curOutlets
        )


_modifyState :: forall state inlets outlets. (state -> state) -> Protocol state inlets outlets -> Effect Unit
_modifyState = flip _.modifyState
