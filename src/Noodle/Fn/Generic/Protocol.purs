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



data InputChange
    = SingleInput InputR -- TODO: HoldsInput
    | AllInputs
    -- TODO: add Hot / Cold


data OutputChange
    = SingleOutput OutputR -- TODO: HoldsOutput
    | AllOutputs


data ChangeFocus
    = Everything
    | StateChange
    | AllInputsChange
    | InputChange InputR -- TODO: HoldsInput
    | AllOutputsChange
    | OutputChange OutputR -- TODO: HoldsOutput


instance Show ChangeFocus where
    show :: ChangeFocus -> String
    show =
        case _ of
            Everything -> "all"
            StateChange -> "state"
            AllInputsChange -> "inputs"
            InputChange inputR -> "input " <> show inputR -- reflect' inputR
            AllOutputsChange -> "outputs"
            OutputChange outputR -> "output " <> show outputR -- reflect' outputR


inputChangeToMaybe :: InputChange -> Maybe InputR
inputChangeToMaybe (SingleInput iid) = Just iid
inputChangeToMaybe AllInputs = Nothing


outputChangeToMaybe :: OutputChange -> Maybe OutputR
outputChangeToMaybe (SingleOutput oid) = Just oid
outputChangeToMaybe AllOutputs = Nothing


type PreUpdatesRow state inputs outputs = (Boolean /\ state) /\ (Boolean /\ InputChange /\ inputs) /\ (Boolean /\ OutputChange /\ outputs)
type PostUpdatesRow state inputs outputs = ChangeFocus /\ PreUpdatesRow state inputs outputs
type FocusedUpdate state inputs outputs = ChangeFocus /\ state /\ inputs /\ outputs


type Protocol state inputs outputs =
    { getInputs :: Unit -> Effect (InputChange /\ inputs)
    , getOutputs :: Unit -> Effect (OutputChange /\ outputs)
    , getState :: Unit -> Effect state
    , modifyInputs :: (inputs -> InputChange /\ inputs) -> Effect Unit
    , modifyOutputs :: (outputs -> OutputChange /\ outputs) -> Effect Unit
    , modifyState :: (state -> state) -> Effect Unit
    }


type Tracker state inputs outputs =
    { state :: Signal state
    , inputs :: Signal (InputChange /\ inputs)
    , outputs :: Signal (OutputChange /\ outputs)
    , all :: Signal (FocusedUpdate state inputs outputs)
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
            inputsInit = true /\ AllInputs /\ inputs
            outputsInit = true /\ AllOutputs /\ outputs

            (initAll :: PostUpdatesRow state inputs outputs) = Everything /\ stateInit /\ inputsInit /\ outputsInit

        stateCh <- channel stateInit
        inputsCh <- channel inputsInit
        outputsCh <- channel outputsInit
        -- allChangesCh <- channel initAll

        let
            stateSig = Channel.subscribe stateCh
            inputsSig = Channel.subscribe inputsCh
            outputsSig = Channel.subscribe outputsCh
            changesSig = Signal.foldp foldUpdates initAll (toPreUpdateRow <$> stateSig <*> inputsSig <*> outputsSig)

            foldUpdates :: PreUpdatesRow state inputs outputs -> PostUpdatesRow state inputs outputs -> PostUpdatesRow state inputs outputs
            foldUpdates
                lastChange@((bState /\ state) /\ (bInput /\ inputChange /\ is) /\ (bOutput /\ outputChange /\ os))
                (_ /\ (bStatePrev /\ _) /\ (bInputPrev /\ _ /\ _) /\ (bOutputPrev /\ _))
                =
                    if bState == not bStatePrev then
                        StateChange /\ lastChange
                    else if bInput == not bInputPrev then
                        case inputChange of
                            AllInputs ->
                                AllInputsChange /\ lastChange
                            SingleInput inputR ->
                                InputChange inputR /\ lastChange
                    else if bOutput == not bOutputPrev then
                        case outputChange of
                            AllOutputs ->
                                AllOutputsChange /\ lastChange
                            SingleOutput outputR ->
                                OutputChange outputR /\ lastChange
                    else Everything /\ lastChange


            toPreUpdateRow
                :: (Boolean /\ state)
                -> (Boolean /\ InputChange /\ inputs)
                -> (Boolean /\ OutputChange /\ outputs)
                -> PreUpdatesRow state inputs outputs
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


inputs :: forall state inputs outputs. Tracker state inputs outputs -> Effect inputs
inputs tracker = Signal.get tracker.inputs <#> Tuple.snd


outputs :: forall state inputs outputs. Tracker state inputs outputs -> Effect outputs
outputs tracker = Signal.get tracker.outputs <#> Tuple.snd


lastInput :: forall state inputs outputs. Tracker state inputs outputs -> Effect (Maybe InputR)
lastInput tracker = Signal.get tracker.inputs <#> Tuple.fst <#> inputChangeToMaybe


lastOutput :: forall state inputs outputs. Tracker state inputs outputs -> Effect (Maybe OutputR)
lastOutput tracker = Signal.get tracker.outputs <#> Tuple.fst <#> outputChangeToMaybe


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
