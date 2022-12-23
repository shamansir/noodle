module Noodle.Fn2.Protocol
  ( Protocol
  , ProtocolS
  , onChannels
  , onRefs
  , onSignals
  , ITest1, ITest2, ITest3, IFnTest1, IFnTest2, IFnTest3
  , OTest1, OTest2, OTest3, OFnTest1, OFnTest2, OFnTest3
  , CurIFn, CurOFn, CurIVal, CurOVal
  )
  where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
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

import Noodle.Fn2.Flow (Input, Output, InputId, OutputId, inputToString, inputId)


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
type CurOFn m = OFnTest3 m


type Protocol state is os m =
    { getInputs :: Unit -> m (Record is)
    , getOutputs :: Unit -> m (Record os)
    , getState :: Unit -> m state
    , modifyInputs :: (Record is -> Record is) -> m Unit
    , modifyOutputs :: (Record os -> Record os) -> m Unit
    , modifyState :: (state -> state) -> m Unit
    , storeLastInput :: CurIFn m
    , storeLastOutput :: CurOFn m
    -- TODO: try `Cons i is is'`
    -- , storeLastInput :: (forall i. IsSymbol i => Maybe (Input i)) -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: (forall o. IsSymbol o => Maybe (Output o)) -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    -- , storeLastInput :: InputId -> m Unit -- could be `InputId`` since we use `Protocol` only internally
    -- , storeLastOutput :: OutputId -> m Unit -- could be `OutputId`` since we use `Protocol` only internally
    }


-- Functor etc., (only for Signal)


type ProtocolS state is os w m =
    { state :: w state
    , inputs :: w (Record is)
    , outputs :: w (Record os)
    -- , lastInput :: w (Maybe InputId)
    -- , lastOutput :: w (Maybe OutputId)
    -- , lastInput :: w (forall i. IsSymbol i => Maybe (Input i))
    -- , lastOutput :: w (forall o. IsSymbol o => Maybe (Output o))
    , lastInput :: w CurIVal
    , lastOutput :: w CurOVal
    , protocol :: Protocol state is os m
    }


type ProtocolW state is os w m =
    m (ProtocolS state is os w m)


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


onChannels
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> ProtocolW state is os Channel m
onChannels state inputs outputs =
    liftEffect $ do

        stateCh <- channel state
        inputsCh <- channel inputs
        outputsCh <- channel outputs
        lastInputCh <- channel $ unsafeCoerce Nothing
        lastOutputCh <- channel $ unsafeCoerce Nothing

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
                        (\maybeInput ->
                                -- (testS :: Maybe String) = -- inputToString <$> (inputUnsafe :: Maybe (forall i. IsSymbol i => Input i))
                                --     case inputUnsafe of
                                --         Just input_ ->
                                --             let (input :: forall i. IsSymbol i => Input i) = unsafeCoerce input_
                                --             in Just (unsafeCoerce (inputToString (unsafeCoerce input)))
                                --         Nothing -> Nothing
                            let
                                (maybeInputUnsafe :: forall i. Maybe (Input i)) = unsafeCoerce maybeInput
                                -- (maybeInputIdUnsafe :: Maybe InputId) = unsafeCoerce (inputId <$> maybeInputUnsafe)
                                --(testS :: Maybe String) = inputToString <$> unsafeCoerce <$> maybeInput
                                -- (testS :: Maybe String) = (unsafeCoerce inputToString :: forall i. IsSymbol i => Input i -> String) <$> ?wh <$> maybeInputUnsafe
                            in liftEffect $ Channel.send lastInputCh $ unsafeCoerce maybeInput
                        )
                    -- :: (Maybe (forall i. Input i)) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\maybeOutput -> liftEffect $ Channel.send lastOutputCh $ unsafeCoerce maybeOutput)
                    -- :: (Maybe (forall o. Output o)) -> m Unit
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