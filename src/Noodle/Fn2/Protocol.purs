module Noodle.Fn2.Protocol
  ( Protocol
  , Tracker
  , make
  , InputChange(..), OutputChange(..)
  , inputs, outputs
  , lastInput, lastOutput
  , ChangeFocus(..)
  , FocusedUpdate
--   , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
--   , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
--   , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

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


{-}
type ITest1 = Maybe (forall i. IsSymbol i => Input i)
type ITest2 = (forall i. IsSymbol i => Maybe (Input i))
type ITest3 = Maybe InputId
type IFnTest1 m = (ITest1 -> m Unit)
type IFnTest2 m = (ITest2 -> m Unit)
type IFnTest3 m = (ITest3 -> m Unit)

type CurIVal = ITest3
type CurIFn m = IFnTest3 m


type OTest1 = Maybe (forall o. IsSymbol o => Output o)
type OTest2 = (forall o. IsSymbol o => Maybe (Output o))
type OTest3 = Maybe OutputId
type OFnTest1 m = (OTest1 -> m Unit)
type OFnTest2 m = (OTest2 -> m Unit)
type OFnTest3 m = (OTest3 -> m Unit)


type CurOVal = OTest3
type CurOFn m = OFnTest3 m -}


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


type Protocol state is os =
    { getInputs :: Unit -> Effect (InputChange /\ Record is)
    , getOutputs :: Unit -> Effect (OutputChange /\ Record os)
    , getState :: Unit -> Effect state
    , modifyInputs :: (Record is -> InputChange /\ Record is) -> Effect Unit
    , modifyOutputs :: (Record os -> OutputChange /\ Record os) -> Effect Unit
    , modifyState :: (state -> state) -> Effect Unit
    -- TODO: try `Cons i is is'`
    -- , storeLastInput :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: (forall o. IsSymbol o => Maybe (Output o)) -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    -- , storeLastInput :: InputId -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: OutputId -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    }


{-
instance StatefulM (Protocol state is os) Effect state where
    getM :: Protocol state is os -> Effect state
    getM = _.getState unit
    setM :: state -> Protocol state is os -> Effect (Protocol state is os)
    setM state proto = do
        proto.modifyState (const state) *> pure proto -}


-- Functor etc., (only for Signal)


-- TODO: all Channel-stuff is `Track`

type Tracker state is os =
    { state :: Signal state
    , inputs :: Signal (InputChange /\ Record is)
    , outputs :: Signal (OutputChange /\ Record os)
    , all :: Signal (FocusedUpdate state is os)
    -- , lastInput :: w (Maybe InputId)
    -- , lastOutput :: w (Maybe OutputId)
    -- , lastInput :: w (forall i. IsSymbol i => Maybe (Input i))
    -- , lastOutput :: w (forall o. IsSymbol o => Maybe (Output o))
    -- , lastInput :: Signal (Maybe InputId)
    -- , lastOutput :: Signal (Maybe OutputId)
    -- , byInput :: Signal (forall din. InputId -> din )
    }


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


type PreUpdatesRow state is os = (Boolean /\ state) /\ (Boolean /\ InputChange /\ Record is) /\ (Boolean /\ OutputChange /\ Record os)
type PostUpdatesRow state is os = ChangeFocus /\ PreUpdatesRow state is os
type FocusedUpdate state is os = ChangeFocus /\ state /\ Record is /\ Record os


make
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os /\ Protocol state is os)
make state inputs outputs =
    liftEffect $ do

         -- boolean flags help to find out which signal was updated the latest in the merged all changes by flipping them on modification

        let
            stateInit = true /\ state
            inputsInit = true /\ AllInputs /\ inputs
            outputsInit = true /\ AllOutputs /\ outputs

            (initAll :: PostUpdatesRow state is os) = Everything /\ stateInit /\ inputsInit /\ outputsInit

        stateCh <- channel stateInit
        inputsCh <- channel inputsInit
        outputsCh <- channel outputsInit
        -- allChangesCh <- channel initAll

        let
            stateSig = Channel.subscribe stateCh
            inputsSig = Channel.subscribe inputsCh
            outputsSig = Channel.subscribe outputsCh
            changesSig = Signal.foldp foldUpdates initAll (toPreUpdateRow <$> stateSig <*> inputsSig <*> outputsSig)

            foldUpdates :: PreUpdatesRow state is os -> PostUpdatesRow state is os -> PostUpdatesRow state is os
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
                -> (Boolean /\ InputChange /\ Record is)
                -> (Boolean /\ OutputChange /\ Record os)
                -> PreUpdatesRow state is os
            toPreUpdateRow = (/\) >>> Tuple.curry

        let
            tracker :: Tracker state is os
            tracker =
                { state : Tuple.snd <$> stateSig
                , inputs : Tuple.snd <$>inputsSig
                , outputs : Tuple.snd <$> outputsSig
                , all : map (bimap Tuple.snd $ bimap (Tuple.snd >>> Tuple.snd) (Tuple.snd >>> Tuple.snd)) <$> changesSig
                }

            protocol :: Protocol state is os
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


inputs :: forall state is os. Tracker state is os -> Effect (Record is)
inputs tracker = Signal.get tracker.inputs <#> Tuple.snd


outputs :: forall state is os. Tracker state is os -> Effect (Record os)
outputs tracker = Signal.get tracker.outputs <#> Tuple.snd


lastInput :: forall state is os. Tracker state is os -> Effect (Maybe InputR)
lastInput tracker = Signal.get tracker.inputs <#> Tuple.fst <#> inputChangeToMaybe


lastOutput :: forall state is os. Tracker state is os -> Effect (Maybe OutputR)
lastOutput tracker = Signal.get tracker.outputs <#> Tuple.fst <#> outputChangeToMaybe


-- type Tracker k v = Ref (k /-> v)


-- newTracker :: forall k v. Effect (Tracker k v)
-- newTracker = Ref.new Map.empty


-- put :: forall k v. Ord k => k -> v -> Tracker k v -> Effect Unit
-- put k v = Ref.modify_ (Map.insert k v)


-- check :: forall k v. Ord k => k -> Tracker k v -> Effect (Maybe v)
-- check k tracker =
--     Ref.read tracker <#> Map.lookup k


{-
mkDefault
    :: forall inputs outputs d
     . Record inputs
    -> Record outputs
    -> Effect (protocol :: Protocol d /\ Record outputs)
mkDefault initials = do
    let initialsMap = Map.fromFoldable initials
    inputs <- newTracker
    outputs <- newTracker
    lastRef <- Ref.new Nothing
    let
        protocol =
            { last : const $ Ref.read lastRef
            , receive : \input -> do
                maybeValAtInput <- check input inputs
                pure $ case maybeValAtInput of
                    Just val -> Just val
                    Nothing -> Map.lookup input initialsMap
            , send : \output val -> put output val outputs
            , sendIn : \input val -> do
                Ref.write (Just input) lastRef
                inputs # put input val
            }
    pure { protocol, inputs, outputs }
-}