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

import Noodle.Id (InputR, OutputR)
import Noodle.Stateful (class StatefulM)

import Noodle.Fn.Generic.Tracker (Tracker)
import Noodle.Fn.Generic.Updates as U


type Protocol state inputs outputs =
    { getInputs :: Unit -> Effect (U.InputChange /\ inputs)
    , getOutputs :: Unit -> Effect (U.OutputChange /\ outputs)
    , getState :: Unit -> Effect state
    , modifyInputs :: (inputs -> U.InputChange /\ inputs) -> Effect Unit
    , modifyOutputs :: (outputs -> U.OutputChange /\ outputs) -> Effect Unit
    , modifyState :: (state -> state) -> Effect Unit
    }


make
    :: forall state inputs outputs m
    .  MonadEffect m
    => state
    -> inputs
    -> outputs
    -> m (Tracker state inputs outputs /\ Protocol state inputs outputs)
make state inputs outputs =
    liftEffect $ do

         -- boolean flags help to find out which signal was updated the latest in the merged all changes by flipping them on modification

        let
            stateInit = true /\ state
            inputsInit = true /\ U.AllInputs /\ inputs
            outputsInit = true /\ U.AllOutputs /\ outputs

            (initAll :: U.PostUpdatesRow state inputs outputs) = U.Everything /\ stateInit /\ inputsInit /\ outputsInit

        stateCh <- channel stateInit
        inputsCh <- channel inputsInit
        outputsCh <- channel outputsInit
        -- allChangesCh <- channel initAll

        let
            stateSig = Channel.subscribe stateCh
            inputsSig = Channel.subscribe inputsCh
            outputsSig = Channel.subscribe outputsCh
            changesSig = Signal.foldp foldUpdates initAll (toPreUpdateRow <$> stateSig <*> inputsSig <*> outputsSig)

            foldUpdates :: U.PreUpdatesRow state inputs outputs -> U.PostUpdatesRow state inputs outputs -> U.PostUpdatesRow state inputs outputs
            foldUpdates
                lastChange@((bState /\ state) /\ (bInput /\ inputChange /\ is) /\ (bOutput /\ outputChange /\ os))
                (_ /\ (bStatePrev /\ _) /\ (bInputPrev /\ _ /\ _) /\ (bOutputPrev /\ _))
                =
                    if bState == not bStatePrev then
                        U.StateChange /\ lastChange
                    else if bInput == not bInputPrev then
                        case inputChange of
                            U.AllInputs ->
                                U.AllInputsChange /\ lastChange
                            U.SingleInput inputR ->
                                U.InputChange inputR /\ lastChange
                    else if bOutput == not bOutputPrev then
                        case outputChange of
                            U.AllOutputs ->
                                U.AllOutputsChange /\ lastChange
                            U.SingleOutput outputR ->
                                U.OutputChange outputR /\ lastChange
                    else U.Everything /\ lastChange


            toPreUpdateRow
                :: (Boolean /\ state)
                -> (Boolean /\ U.InputChange /\ inputs)
                -> (Boolean /\ U.OutputChange /\ outputs)
                -> U.PreUpdatesRow state inputs outputs
            toPreUpdateRow = (/\) >>> Tuple.curry

        let
            tracker :: Tracker state inputs outputs
            tracker =
                { state : Tuple.snd <$> stateSig
                , inputs : Tuple.snd <$>inputsSig
                , outputs : Tuple.snd <$> outputsSig
                , all : map (bimap Tuple.snd $ bimap (Tuple.snd >>> Tuple.snd) (Tuple.snd >>> Tuple.snd)) <$> changesSig
                }

            protocol :: Protocol state inputs outputs
            protocol =
                { getInputs : const $ liftEffect $ Signal.get $ Tuple.snd <$> inputsSig
                , getOutputs : const $ liftEffect $ Signal.get $ Tuple.snd <$> outputsSig
                , getState : const $ liftEffect $ Signal.get $ Tuple.snd <$> stateSig
                -- bwlow we flip the flags on every update in particular signal
                , modifyInputs : \f -> liftEffect $ Signal.get inputsSig >>= bimap not (Tuple.snd >>> f) >>> Channel.send inputsCh
                , modifyOutputs : \f -> liftEffect $ Signal.get outputsSig >>= bimap not (Tuple.snd >>> f) >>> Channel.send outputsCh -- Tuple.snd >>> f >>> Channel.send outputsCh
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
onRefs state inputs outputs =
    liftEffect $ do

        stateRef <- Ref.new state
        inputsRef <- Ref.new inputs
        outputsRef <- Ref.new outputs
        -- (lastInputRef  :: Ref (forall i. IsSymbol i => Maybe (Input i))) <- Ref.new $ unsafeCoerce Nothing
        -- (lastOutputRef  :: Ref (forall o. IsSymbol o => Maybe (Output o))) <- Ref.new $ unsafeCoerce Nothing
        (lastInputRef  :: Ref CurIVal) <- Ref.new $ unsafeCoerce Nothing
        (lastOutputRef  :: Ref CurOVal) <- Ref.new $ unsafeCoerce Nothing

        pure
            { state : stateRef
            , inputs : inputsRef
            , outputs : outputsRef
            , lastInput : lastInputRef
            , lastOutput : lastOutputRef
            , protocol :
                { getInputs : const $ liftEffect $ Ref.read inputsRef
                , getOutputs : const $ liftEffect $ Ref.read outputsRef
                , getState : const $ liftEffect $ Ref.read stateRef
                , modifyInputs : \f -> liftEffect $ Ref.modify_ f inputsRef
                , modifyOutputs : \f -> liftEffect $ Ref.modify_ f outputsRef
                , modifyState : \f -> liftEffect $ Ref.modify_ f stateRef
                , storeLastInput :
                    (
                        (\maybeInput -> liftEffect (Ref.write (unsafeCoerce maybeInput) lastInputRef))
                        -- (\maybeInput -> liftEffect (Ref.write (unsafeCoerce <$> maybeInput) lastInputRef))
                    -- :: (Maybe (forall i. IsSymbol i => Input i)) -> m Unit
                    -- :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit
                    -- :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\maybeOutput -> liftEffect $ Ref.write (unsafeCoerce maybeOutput) lastOutputRef)
                        -- (\maybeOutput -> liftEffect $ Ref.write (unsafeCoerce <$> maybeOutput) lastOutputRef)
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
onSignals state inputs outputs = do
    onChannelsI <- onChannels state inputs outputs
    liftEffect $ do

        pure
            { state : Channel.subscribe onChannelsI.state
            , inputs : Channel.subscribe onChannelsI.inputs
            , outputs : Channel.subscribe onChannelsI.outputs
            , lastInput : Channel.subscribe onChannelsI.lastInput
            , lastOutput : Channel.subscribe onChannelsI.lastOutput
            , protocol : onChannelsI.protocol
            }

-}
