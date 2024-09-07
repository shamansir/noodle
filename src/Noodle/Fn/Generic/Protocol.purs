module Noodle.Fn.Generic.Protocol where

import Prelude

import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Signal (Signal)
import Signal as Signal
import Signal.Channel (channel)
import Signal.Channel (subscribe, send) as Channel
import Signal.Extra ((>*<))

import Noodle.Id (InletR, OutletR)

import Noodle.Fn.Generic.Tracker (Tracker)
import Noodle.Fn.Generic.Updates as U


type Protocol state inlets outlets =
    { getInlets :: Unit -> Effect (U.InletsUpdate /\ inlets)
    , getOutlets :: Unit -> Effect (U.OutletsUpdate /\ outlets)
    , getState :: Unit -> Effect state
    , modifyInlets :: (inlets -> U.InletsUpdate /\ inlets) -> Effect Unit
    , modifyOutlets :: (outlets -> U.OutletsUpdate /\ outlets) -> Effect Unit
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

        let
            stateInit   = state
            inletsInit  = U.AllInlets /\ inlets
            outletsInit = U.AllOutlets /\ outlets

            (initAll :: U.Update state inlets outlets) = U.UpdateEverything state inlets outlets

        stateCh   <- channel stateInit
        inletsCh  <- channel inletsInit
        outletsCh <- channel outletsInit
        -- allChangesCh <- channel initAll

        let
            stateSig   = Channel.subscribe stateCh
            inletsSig  = Channel.subscribe inletsCh
            outletsSig = Channel.subscribe outletsCh
            updatesSig :: Signal (U.Update state inlets outlets)
            updatesSig = Signal.constant initAll
                            >*< (U.UpdateState <$> stateSig)
                            >*< (Tuple.uncurry U.UpdateInlets <$> inletsSig)
                            >*< (Tuple.uncurry U.UpdateOutlets <$> outletsSig)

        let
            tracker :: Tracker state inlets outlets
            tracker =
                { state   : stateSig
                , inlets  : inletsSig
                , outlets : outletsSig
                , all     : Signal.foldp U.fold (U.startCollecting state inlets outlets) updatesSig
                }

            protocol :: Protocol state inlets outlets
            protocol =
                { getInlets : const $ Signal.get inletsSig
                , getOutlets : const $ Signal.get outletsSig
                , getState : const $ Signal.get stateSig
                , modifyInlets : \f -> Signal.get inletsSig >>= (Tuple.snd >>> f) >>> Channel.send inletsCh -- TODO: use Signal.sampleOn?
                , modifyOutlets : \f -> Signal.get outletsSig >>= (Tuple.snd >>> f) >>> Channel.send outletsCh -- TODO: use Signal.sampleOn?
                , modifyState : \f -> Signal.get stateSig  >>= f >>> Channel.send stateCh
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


imapState :: forall state state' inlets outlets. (state -> state') -> (state' -> state) -> Protocol state inlets outlets -> Protocol state' inlets outlets
imapState f g { getInlets, getOutlets, getState, modifyInlets, modifyOutlets, modifyState } =
    { getInlets
    , getOutlets
    , getState : map f <$> getState
    , modifyInlets
    , modifyOutlets
    , modifyState : \modifyF -> modifyState (g <<< modifyF <<< f)
    }


imapInlets :: forall state inlets inlets' outlets. (inlets -> inlets') -> (inlets' -> inlets) -> Protocol state inlets outlets -> Protocol state inlets' outlets
imapInlets f g { getInlets, getOutlets, getState, modifyInlets, modifyOutlets, modifyState } =
    { getInlets : map (map f) <$> getInlets
    , getOutlets
    , getState
    , modifyInlets : \modifyF -> modifyInlets (map g <<< modifyF <<< f)
    , modifyOutlets
    , modifyState
    }


imapOutlets :: forall state inlets outlets outlets'. (outlets -> outlets') -> (outlets' -> outlets) -> Protocol state inlets outlets -> Protocol state inlets outlets'
imapOutlets f g { getInlets, getOutlets, getState, modifyInlets, modifyOutlets, modifyState } =
    { getInlets
    , getOutlets : map (map f) <$> getOutlets
    , getState
    , modifyInlets
    , modifyOutlets : \modifyF -> modifyOutlets (map g <<< modifyF <<< f)
    , modifyState
    }