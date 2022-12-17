module Noodle.Fn2.Protocol
  ( Protocol
  , onRefs
  , onSignals
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





class Channel a d | d -> a where
    adapt :: d -> Maybe a
    lift :: a -> d


type Protocol is os state m =
    { getInputs :: Unit -> m (Record is)
    , getOutputs :: Unit -> m (Record os)
    , getState :: Unit -> m state
    , modifyInputs :: (Record is -> Record is) -> m Unit
    , modifyOutputs :: (Record os -> Record os) -> m Unit
    , modifyState :: (state -> state) -> m Unit
    , storeLastInput :: (forall proxy i. IsSymbol i => Maybe (proxy i) -> m Unit)
    , storeLastOutput :: (forall proxy o. IsSymbol o => Maybe (proxy o) -> m Unit)
    }



onRefs
    :: forall state is os m
    .  MonadEffect m
    => Ref state
    -> Ref (Record is)
    -> Ref (Record os)
    -> Ref (forall iproxy i. IsSymbol i => Maybe (iproxy i))
    -> Ref (forall oproxy o. IsSymbol o => Maybe (oproxy o))
    -> Protocol is os state m
onRefs stateRef inputsRef outputsRef saveLastInputRef saveLastOutputRef =
    { getInputs : const $ liftEffect $ Ref.read inputsRef
    , getOutputs : const $ liftEffect $ Ref.read outputsRef
    , getState : const $ liftEffect $ Ref.read stateRef
    , modifyInputs : \f -> liftEffect $ Ref.modify_ f inputsRef
    , modifyOutputs : \f -> liftEffect $ Ref.modify_ f outputsRef
    , modifyState : \f -> liftEffect $ Ref.modify_ f stateRef
    , storeLastInput : \input -> liftEffect $ Ref.write (unsafeCoerce input) saveLastInputRef
    , storeLastOutput : \output -> liftEffect $ Ref.write (unsafeCoerce output) saveLastOutputRef
    }


onSignals
    :: forall state is os m
    .  MonadEffect m
    => state
    -> Record is
    -> Record os
    -> m { protocol :: Protocol is os state m }
onSignals state inputs outputs =
    liftEffect $ do

        stateCh <- channel state
        inputsCh <- channel inputs
        outputsCh <- channel outputs
        (lastInputCh :: Channel (forall iproxy i. IsSymbol i => Maybe (iproxy i))) <- channel $ unsafeCoerce Nothing
        (lastOutputCh :: Channel (forall oproxy o. IsSymbol o => Maybe (oproxy o))) <- channel $ unsafeCoerce Nothing

        let stateSig = Channel.subscribe stateCh
        let inputsSig = Channel.subscribe inputsCh
        let outputsSig = Channel.subscribe outputsCh
        let saveLastInputSig = Channel.subscribe lastInputCh
        let saveLastOutputSig = Channel.subscribe lastOutputCh

        pure { protocol :
                { getInputs : const $ liftEffect $ Signal.get inputsSig
                , getOutputs : const $ liftEffect $ Signal.get outputsSig
                , getState : const $ liftEffect $ Signal.get stateSig
                , modifyInputs : \f -> liftEffect $ Signal.get inputsSig >>= \v -> Channel.send inputsCh (f v)
                , modifyOutputs : \f -> liftEffect $ Signal.get outputsSig >>= \v -> Channel.send outputsCh (f v)
                , modifyState : \f -> liftEffect $ Signal.get stateSig >>= \v -> Channel.send stateCh (f v)
                , storeLastInput :
                    (
                        (\input -> liftEffect $ Channel.send lastInputCh $ unsafeCoerce input)
                    :: forall proxy i. IsSymbol i => Maybe (proxy i) -> m Unit
                    )
                , storeLastOutput :
                    (
                        (\output -> liftEffect $ Channel.send lastOutputCh $ unsafeCoerce output)
                    :: forall proxy o. IsSymbol o => Maybe (proxy o) -> m Unit
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