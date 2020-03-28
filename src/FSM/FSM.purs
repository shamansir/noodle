module FSM
    ( FSM(..)
    ) where

import Prelude (Unit, bind, (<$>), (>>=), (<>), pure, identity)
import Effect (Effect)

import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)
import Data.Traversable (traverse_)

import FSM.Covered (Covered)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Util (Canceler)


data FSM model action =
    FSM model (action -> model -> model /\ Array (Effect action))


type CoveredFSM model error action = FSM (Covered error model) action


prepare
    :: forall model action
     . FSM model action
    -> Effect
            { models :: Event model
            , actions :: Event action
            , pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
prepare (FSM init f) = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (model /\ Array (Effect action))) =
            Event.fold
                (\action prev -> f action (fst prev))
                actions
                (init /\ [])
        (models :: Event model)
            = fst <$> updates
    stopPerformingEffects <- Event.subscribe updates \(_ /\ effects) ->
        traverse_ (\eff -> eff >>= pushAction) effects
    pure { models, pushAction, actions, stop : stopPerformingEffects }


run
    :: forall model action
     . FSM model action
    -> Array action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm actionList = do
    { models, actions, pushAction, stop } <- prepare fsm
    _ <- traverse_ pushAction actionList
    pure { pushAction, stop : stop }


runWith
    :: forall model action
     . FSM model action
    -> (model -> Effect Unit)
    -> Array action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
runWith fsm stepHandler actionList = do
    { models, actions, pushAction, stop } <- prepare fsm
    stopInforming <- Event.subscribe models stepHandler
    _ <- traverse_ pushAction actionList
    -- _ <- stopInforming
    pure { pushAction, stop : stopInforming <> stop }
