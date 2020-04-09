module FSM
    ( FSM(..) -- FIXME: do not expose constructor
    , prepare -- FIXME: do not expose
    , make, makePassing
    , run, run', fold
    , pushAll
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Console as Console

import Data.List (List)
import Data.List as List
import Data.Foldable (class Foldable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)
import Data.Traversable (traverse_)


import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Util (Canceler)


data FSM action model =
    -- FIXME: try: (action -> model -> Effect (model /\ Array action))
    FSM (action -> model -> model /\ Effect action)
    -- Array -> Foldable & Applicative & Monoid
    -- FIXME: we don't need an `Array` if there's an `action` Like `Batch (Array (Effect action))`


make
    :: forall action model
     . (action -> model -> model /\ Effect action)
    -> FSM action model
make = FSM


makePassing
    :: forall action model
     . Monoid action
    => FSM action model
makePassing = FSM (\_ m -> m /\ pure mempty)


prepare
    :: forall action model
     . Monoid action
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
prepare (FSM f) init subscription = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (model /\ Effect action)) =
            Event.fold
                (\action prev -> f action $ fst prev)
                actions
                (init /\ pure mempty)
        (models :: Event model)
            = fst <$> updates
    stopSubscription <- Event.subscribe models subscription
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ eff) -> eff >>= pushAction
    pure { pushAction, stop : stopSubscription <> stopPerformingEffects }


run
    :: forall action model
     . Monoid action
    => FSM action model
    -> model
    -> List action -- FIXME: use foldable
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm init = do
    run' fsm init $ const $ pure unit


run'
    :: forall action model
     . Monoid action
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> List action -- FIXME: use foldable
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run' fsm init subscription actionList = do
    { pushAction, stop } <- prepare fsm init subscription
    _ <- traverse_ pushAction actionList
    pure { pushAction, stop : stop }


fold
    :: forall action model
     . Monoid action
    => FSM action model
    -> model
    -> List action
    -> Effect (model /\ Canceler)
fold fsm init actionList = do
    lastValRef <- Ref.new init
    { pushAction, stop } <- prepare fsm init $ flip Ref.write lastValRef
    _ <- traverse_ pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ stop


pushAll :: forall action. (action -> Effect Unit) -> List action -> Effect Unit
pushAll = traverse_
