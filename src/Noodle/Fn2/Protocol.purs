module Noodle.Fn2.Protocol
  ( Protocol
  , ProtocolS
  , onRefs
  , onSignals
  , onChannels
  )
  where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal)
import Signal as Signal
import Signal.Channel (Channel, channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)

import Noodle.Fn2.Flow (Input, Output)


type Protocol is os state m =
    { getInputs :: Unit -> m (Record is)
    , getOutputs :: Unit -> m (Record os)
    , getState :: Unit -> m state
    , modifyInputs :: (Record is -> Record is) -> m Unit
    , modifyOutputs :: (Record os -> Record os) -> m Unit
    , modifyState :: (state -> state) -> m Unit
    , storeLastInput :: (Maybe (forall i. Input i) -> m Unit)
    , storeLastOutput :: (Maybe (forall o. Output o) -> m Unit)
    }


type ProtocolS is os state w m =
    { state :: w state
    , inputs :: w (Record is)
    , outputs :: w (Record os)
    , lastInput :: w (Maybe (forall i. Input i))
    , lastOutput :: w (Maybe (forall o. Output o))
    , protocol :: Protocol is os state m
    }


type ProtocolW is os state w m =
    m (ProtocolS is os state w m)


onRefs
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW is os state Ref m
onRefs state inputs outputs =
    liftEffect $ do

        stateRef <- Ref.new state
        inputsRef <- Ref.new inputs
        outputsRef <- Ref.new outputs
        (lastInputRef :: Ref (Maybe (forall i. Input i))) <- Ref.new $ unsafeCoerce Nothing
        (lastOutputRef :: Ref (Maybe (forall o. Output o))) <- Ref.new $ unsafeCoerce Nothing

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
                        (\input -> liftEffect $ Ref.write (unsafeCoerce input) lastInputRef)
                    :: (Maybe (forall i. Input i)) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\output -> liftEffect $ Ref.write (unsafeCoerce output) lastOutputRef)
                    :: (Maybe (forall o. Output o)) -> m Unit
                    )
                }
            }



onSignals
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW is os state Signal m
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


onChannels
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW is os state Channel m
onChannels state inputs outputs =
    liftEffect $ do

        stateCh <- channel state
        inputsCh <- channel inputs
        outputsCh <- channel outputs
        (lastInputCh :: Channel (Maybe (forall i. Input i))) <- channel $ unsafeCoerce Nothing
        (lastOutputCh :: Channel (Maybe (forall o. Output o))) <- channel $ unsafeCoerce Nothing

        let stateSig = Channel.subscribe stateCh
        let inputsSig = Channel.subscribe inputsCh
        let outputsSig = Channel.subscribe outputsCh
        -- let saveLastInputSig = Channel.subscribe lastInputCh
        -- let saveLastOutputSig = Channel.subscribe lastOutputCh

        pure
            { state : stateCh
            , inputs : inputsCh
            , outputs : outputsCh
            , lastInput : lastInputCh
            , lastOutput : lastOutputCh
            , protocol :
                { getInputs : const $ liftEffect $ Signal.get inputsSig
                , getOutputs : const $ liftEffect $ Signal.get outputsSig
                , getState : const $ liftEffect $ Signal.get stateSig
                , modifyInputs : \f -> liftEffect $ Signal.get inputsSig >>= \v -> Channel.send inputsCh (f v)
                , modifyOutputs : \f -> liftEffect $ Signal.get outputsSig >>= \v -> Channel.send outputsCh (f v)
                , modifyState : \f -> liftEffect $ Signal.get stateSig >>= \v -> Channel.send stateCh (f v)
                , storeLastInput :
                    (
                        (\input -> liftEffect $ Channel.send lastInputCh $ unsafeCoerce input)
                    :: (Maybe (forall i. Input i)) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\output -> liftEffect $ Channel.send lastOutputCh $ unsafeCoerce output)
                    :: (Maybe (forall o. Output o)) -> m Unit
                    )
                }
            }


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