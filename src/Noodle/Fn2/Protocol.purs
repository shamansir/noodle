module Noodle.Fn2.Protocol
  ( Protocol
  , onRefs
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
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Unsafe.Coerce (unsafeCoerce)





class Channel a d | d -> a where
    adapt :: d -> Maybe a
    lift :: a -> d


type Protocol is os state m =
    { getInputs :: Unit -> m is
    , getOutputs :: Unit -> m os
    , getState :: Unit -> m state
    , modifyInputs :: (is -> is) -> m Unit
    , modifyOutputs :: (os -> os) -> m Unit
    , modifyState :: (state -> state) -> m Unit
    , storeLastInput :: (forall proxy i. IsSymbol i => proxy i -> m Unit)
    , storeLastOutput :: (forall proxy o. IsSymbol o => proxy o -> m Unit)
    }



onRefs :: forall state is os m. MonadEffect m => Ref state -> Ref is -> Ref os -> Ref (forall iproxy i. IsSymbol i => iproxy i) -> Ref (forall oproxy o. IsSymbol o => oproxy o) -> Protocol is os state m
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


onSignals :: forall state is os m. MonadEffect m => state -> is -> os -> { protocol :: Protocol is os state m }
onSignals state inputs outputs saveLastInput saveLastOutput =
    { protocol }
    where
        stateSig = Channel.subscribe state
        inputsSig = Channel.subscribe inputs
        outputsSig = Channel.subscribe outputs
        protocol =
            { getInputs : const $ liftEffect $ Signal.get inputsSig
            , getOutputs : const $ liftEffect $ Signal.get outputsSig
            , getState : const $ liftEffect $ Signal.get stateSig
            , modifyInputs : \f -> liftEffect $ map f inputsSig -- Signal.get >>= \v -> Signal.  Ref.modify_ f inputsRef
            , modifyOutputs : \f -> liftEffect $ map f outputsSig
            , modifyState : \f -> liftEffect $ map f stateSig
            , storeLastInput : \input -> liftEffect $ Ref.write (unsafeCoerce input) saveLastInputSig
            , storeLastOutput : \output -> liftEffect $ Ref.write (unsafeCoerce output) saveLastOutputSig
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