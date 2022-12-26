module Noodle.Fn2.Protocol
  ( Protocol
  , Tracker
  , onChannels
  , InputChange(..), OutputChange(..)
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

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)

import Noodle.Fn2.Flow (Input, Output, InputId, OutputId, inputToString, inputId)


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
    = SingleInput InputId
    | AllInputs


data OutputChange
    = SingleOutput OutputId
    | AllOutputs


type Protocol state is os m =
    { getInputs :: Unit -> m (InputChange /\ Record is)
    , getOutputs :: Unit -> m (OutputChange /\ Record os)
    , getState :: Unit -> m state
    , modifyInputs :: (Record is -> InputChange /\ Record is) -> m Unit
    , modifyOutputs :: (Record os -> OutputChange /\ Record os) -> m Unit
    , modifyState :: (state -> state) -> m Unit
    -- TODO: try `Cons i is is'`
    -- , storeLastInput :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: (forall o. IsSymbol o => Maybe (Output o)) -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    -- , storeLastInput :: InputId -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: OutputId -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    }


-- Functor etc., (only for Signal)


-- TODO: all Channel-stuff is `Track`

type Tracker state is os =
    { state :: Signal state
    , inputs :: Signal (InputChange /\ Record is)
    , outputs :: Signal (OutputChange /\ Record os)
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


onChannels
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m (Tracker state is os /\ Protocol state is os m)
onChannels state inputs outputs =
    liftEffect $ do

        stateCh <- channel state
        inputsCh <- channel (AllInputs /\ inputs)
        outputsCh <- channel (AllOutputs /\ outputs)

        let
            stateSig = Channel.subscribe stateCh
            inputsSig = Channel.subscribe inputsCh
            outputsSig = Channel.subscribe outputsCh
            tracker :: Tracker state is os
            tracker =
                { state : stateSig
                , inputs : inputsSig
                , outputs : outputsSig
                }
            protocol :: Protocol state is os m
            protocol =
                { getInputs : const $ liftEffect $ Signal.get inputsSig
                , getOutputs : const $ liftEffect $ Signal.get outputsSig
                , getState : const $ liftEffect $ Signal.get stateSig
                , modifyInputs : \f -> liftEffect $ Signal.get inputsSig >>= Tuple.snd >>> f >>> Channel.send inputsCh
                , modifyOutputs : \f -> liftEffect $ Signal.get outputsSig >>= Tuple.snd >>> f >>> Channel.send outputsCh
                , modifyState : \f -> liftEffect $ Signal.get stateSig >>= f >>> Channel.send stateCh
                }

        pure $ tracker /\ protocol


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