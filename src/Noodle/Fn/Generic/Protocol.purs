module Noodle.Fn.Generic.Protocol where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.SProxy (reflect, reflect')

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)

import Noodle.Id (class HasInput, class HasOutput, InletR, OutletR, Input, Input', Output, Output', inputR, inputR', outputR, outputR')
import Noodle.Stateful (class StatefulM)

import Noodle.Fn.Generic.Tracker (Tracker)
import Noodle.Fn.Generic.Updates as U


type Protocol state inlets outlets =
    { getInputs :: Unit -> Effect (U.InletsChange /\ inlets)
    , getOutputs :: Unit -> Effect (U.OutletsChange /\ outlets)
    , getState :: Unit -> Effect state
    , modifyInputs :: (inlets -> U.InletsChange /\ inlets) -> Effect Unit
    , modifyOutputs :: (outlets -> U.OutletsChange /\ outlets) -> Effect Unit
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
            stateInit = true /\ state
            inletsInit = true /\ U.AllInlets /\ inlets
            outletsInit = true /\ U.AllOutlets /\ outlets

            (initAll :: U.PostUpdatesRow state inlets outlets) = U.Everything /\ stateInit /\ inletsInit /\ outletsInit

        stateCh <- channel stateInit
        inletsCh <- channel inletsInit
        outletsCh <- channel outletsInit
        -- allChangesCh <- channel initAll

        let
            stateSig = Channel.subscribe stateCh
            inletsSig = Channel.subscribe inletsCh
            outletsSig = Channel.subscribe outletsCh
            changesSig = Signal.foldp foldUpdates initAll (toPreUpdateRow <$> stateSig <*> inletsSig <*> outletsSig)

            foldUpdates :: U.PreUpdatesRow state inlets outlets -> U.PostUpdatesRow state inlets outlets -> U.PostUpdatesRow state inlets outlets
            foldUpdates
                lastChange@((bState /\ state) /\ (bInput /\ inputChange /\ is) /\ (bOutput /\ outputChange /\ os))
                (_ /\ (bStatePrev /\ _) /\ (bInputPrev /\ _ /\ _) /\ (bOutputPrev /\ _))
                =
                    if bState == not bStatePrev then
                        U.StateChange /\ lastChange
                    else if bInput == not bInputPrev then
                        case inputChange of
                            U.AllInlets ->
                                U.AllInletsChange /\ lastChange
                            U.SingleInlet inputR ->
                                U.InletsChange inputR /\ lastChange
                    else if bOutput == not bOutputPrev then
                        case outputChange of
                            U.AllOutlets ->
                                U.AllOutletsChange /\ lastChange
                            U.SingleOutlet outputR ->
                                U.OutletsChange outputR /\ lastChange
                    else U.Everything /\ lastChange


            toPreUpdateRow
                :: (Boolean /\ state)
                -> (Boolean /\ U.InletsChange /\ inlets)
                -> (Boolean /\ U.OutletsChange /\ outlets)
                -> U.PreUpdatesRow state inlets outlets
            toPreUpdateRow = (/\) >>> Tuple.curry

        let
            tracker :: Tracker state inlets outlets
            tracker =
                { state : Tuple.snd <$> stateSig
                , inlets : Tuple.snd <$>inletsSig
                , outlets : Tuple.snd <$> outletsSig
                , all : map (bimap Tuple.snd $ bimap (Tuple.snd >>> Tuple.snd) (Tuple.snd >>> Tuple.snd)) <$> changesSig
                }

            protocol :: Protocol state inlets outlets
            protocol =
                { getInputs : const $ liftEffect $ Signal.get $ Tuple.snd <$> inletsSig
                , getOutputs : const $ liftEffect $ Signal.get $ Tuple.snd <$> outletsSig
                , getState : const $ liftEffect $ Signal.get $ Tuple.snd <$> stateSig
                -- bwlow we flip the flags on every update in particular signal
                , modifyInputs : \f -> liftEffect $ Signal.get inletsSig >>= bimap not (Tuple.snd >>> f) >>> Channel.send inletsCh
                , modifyOutputs : \f -> liftEffect $ Signal.get outletsSig >>= bimap not (Tuple.snd >>> f) >>> Channel.send outletsCh -- Tuple.snd >>> f >>> Channel.send outletsCh
                , modifyState : \f -> liftEffect $ Signal.get stateSig >>= bimap not f >>> Channel.send stateCh -- f >>> Channel.send stateCh
                }

        pure $ tracker /\ protocol

{-
onRefs
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW state is os Ref m
onRefs state inlets outlets =
    liftEffect $ do

        stateRef <- Ref.new state
        inletsRef <- Ref.new inlets
        outletsRef <- Ref.new outlets
        -- (lastInletRef  :: Ref (forall i. IsSymbol i => Maybe (Input i))) <- Ref.new $ unsafeCoerce Nothing
        -- (lastOutletRef  :: Ref (forall o. IsSymbol o => Maybe (Output o))) <- Ref.new $ unsafeCoerce Nothing
        (lastInletRef  :: Ref CurIVal) <- Ref.new $ unsafeCoerce Nothing
        (lastOutletRef  :: Ref CurOVal) <- Ref.new $ unsafeCoerce Nothing

        pure
            { state : stateRef
            , inlets : inletsRef
            , outlets : outletsRef
            , lastInput : lastInletRef
            , lastOutput : lastOutletRef
            , protocol :
                { getInputs : const $ liftEffect $ Ref.read inletsRef
                , getOutputs : const $ liftEffect $ Ref.read outletsRef
                , getState : const $ liftEffect $ Ref.read stateRef
                , modifyInputs : \f -> liftEffect $ Ref.modify_ f inletsRef
                , modifyOutputs : \f -> liftEffect $ Ref.modify_ f outletsRef
                , modifyState : \f -> liftEffect $ Ref.modify_ f stateRef
                , storeLastInput :
                    (
                        (\maybeInput -> liftEffect (Ref.write (unsafeCoerce maybeInput) lastInletRef))
                        -- (\maybeInput -> liftEffect (Ref.write (unsafeCoerce <$> maybeInput) lastInletRef))
                    -- :: (Maybe (forall i. IsSymbol i => Input i)) -> m Unit
                    -- :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit
                    -- :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\maybeOutput -> liftEffect $ Ref.write (unsafeCoerce maybeOutput) lastOutletRef)
                        -- (\maybeOutput -> liftEffect $ Ref.write (unsafeCoerce <$> maybeOutput) lastOutletRef)
                    -- :: (Maybe (forall o. IsSymbol o => Output o)) -> m Unit
                    )
                }
            }



onSignals
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW state is os Signal m
onSignals state inlets outlets = do
    onChannelsI <- onChannels state inlets outlets
    liftEffect $ do

        pure
            { state : Channel.subscribe onChannelsI.state
            , inlets : Channel.subscribe onChannelsI.inlets
            , outlets : Channel.subscribe onChannelsI.outlets
            , lastInput : Channel.subscribe onChannelsI.lastInput
            , lastOutput : Channel.subscribe onChannelsI.lastOutput
            , protocol : onChannelsI.protocol
            }

-}


_modifyInput :: forall state inlets outlets. (inlets -> inlets) -> InletR -> Protocol state inlets outlets -> Effect Unit
_modifyInput f input protocol =
    protocol.modifyInputs
        (\curInputs ->
            (U.SingleInlet $ input) /\ f curInputs
        )


_modifyOutput :: forall state inlets outlets. (outlets -> outlets) -> OutletR -> Protocol state inlets outlets -> Effect Unit
_modifyOutput f output protocol =
    protocol.modifyOutputs
        (\curOutputs ->
            (U.SingleOutlet output) /\ f curOutputs
        )


_modifyState :: forall state inlets outlets. (state -> state) -> Protocol state inlets outlets -> Effect Unit
_modifyState = flip _.modifyState
