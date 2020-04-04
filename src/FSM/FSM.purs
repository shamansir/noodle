module FSM
    ( FSM(..) -- FIXME: do not expose constructor
    , prepare -- FIXME: do not expose
    , make, makePassing
    , run, runAndSubscribe, fold
    , pushAll
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref

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
    FSM (action -> model -> model /\ Effect (Array action))
    -- Array -> Foldable & Applicative
    -- FIXME: we don't need an `Array` if there's an `action` Like `Batch (Array (Effect action))`


make
    :: forall action model
     . (action -> model -> model /\ Effect (Array action))
    -> FSM action model
make = FSM


makePassing
    :: forall action model
     . FSM action model
makePassing = FSM (\_ m -> m /\ pure [])


prepare
    :: forall action model
     . FSM action model
    -> model
    -> Effect
            { models :: Event model
            , actions :: Event action
            , pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
prepare (FSM f) init = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (model /\ Effect (Array action))) =
            Event.fold
                (\action prev -> f action (fst prev))
                actions
                (init /\ pure [])
        (models :: Event model)
            = fst <$> updates
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ eff) -> eff >>= traverse_ pushAction
    pure { models, pushAction, actions, stop : stopPerformingEffects }


run
    :: forall action model
     . FSM action model
    -> model
    -> List action -- FIXME: use foldable
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm init actionList = do
    { models, actions, pushAction, stop } <- prepare fsm init
    _ <- traverse_ pushAction actionList
    pure { pushAction, stop : stop }


runAndSubscribe
    :: forall action model
     . FSM action model
    -> model
    -> (model -> Effect Unit)
    -> List action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
runAndSubscribe fsm init subscription actionList = do
    { models, actions, pushAction, stop } <- prepare fsm init
    stopInforming <- Event.subscribe models subscription
    _ <- traverse_ pushAction actionList
    -- _ <- stopInforming
    pure { pushAction, stop : stopInforming <> stop }


fold
    :: forall action model
     . FSM action model
    -> model
    -> List action
    -> Effect
            (model /\
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            })
fold fsm init actionList = do
    res@{ models, pushAction, stop } <- prepare fsm init
    lastValRef <- Ref.new init
    let modelsFolded =
            Event.fold (\next _ -> next) models init
    stopCollectingLastValue <-
        Event.subscribe modelsFolded (flip Ref.write lastValRef)
    _ <- pushAll pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ { pushAction, stop : stop <> stopCollectingLastValue }


pushAll :: forall action. (action -> Effect Unit) -> List action -> Effect Unit
pushAll = traverse_


-- TODO: run tracing actions

---TODO: run tracing errors (CoveredFSM)

-- TODO: run collection errors to array (CoveredFSM)

-- TODO: covered FSM: stop at first error?
