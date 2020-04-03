module FSM
    ( FSM(..) -- FIXME: do not expose constructor
    , prepare -- FIXME: do not expose
    , make, makePassing
    , run, runAndSubscribe--, runFolding
    , pushAll
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref

import Data.List (List)
import Data.List as List
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)
import Data.Traversable (traverse_)


import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Util (Canceler)


data FSM action model =
    FSM model (action -> model -> model /\ Array (Effect action))
    -- FIXME: we don't need an `Array` if there's an `action` Like `Batch (Array (Effect action))`


make
    :: forall action model
     . model
    -> (action -> model -> model /\ Array (Effect action))
    -> FSM action model
make = FSM


makePassing
    :: forall action model
     . model
    -> FSM action model
makePassing model = FSM model (\_ m -> m /\ [])


prepare
    :: forall action model
     . FSM action model
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
    :: forall action model
     . FSM action model
    -> List action -- FIXME: use foldable
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm actionList = do
    { models, actions, pushAction, stop } <- prepare fsm
    _ <- traverse_ pushAction actionList
    pure { pushAction, stop : stop }


runAndSubscribe
    :: forall action model
     . FSM action model
    -> (model -> Effect Unit)
    -> List action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
runAndSubscribe fsm subscription actionList = do
    { models, actions, pushAction, stop } <- prepare fsm
    stopInforming <- Event.subscribe models subscription
    _ <- traverse_ pushAction actionList
    -- _ <- stopInforming
    pure { pushAction, stop : stopInforming <> stop }


{-
runFolding
    :: forall error action model
     . FSM error action model
    -> List action
    -> Effect
            ((List error /\ model) /\
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            })
runFolding fsm@(FSM initial _) actionList = do
    res@{ models, pushAction, stop } <- prepare fsm
    lastValRef <- Ref.new initialCovered
    let modelsFolded =
            Event.fold appendError models initialCovered
    stopCollectingLastValue <-
        Event.subscribe modelsFolded (flip Ref.write lastValRef)
    _ <- pushAll pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ uncover' lastVal /\ { pushAction, stop : stop <> stopCollectingLastValue }
    where initialCovered = mapError List.singleton initial
-}


pushAll :: forall action. (action -> Effect Unit) -> List action -> Effect Unit
pushAll = traverse_


-- TODO: run tracing actions

---TODO: run tracing errors (CoveredFSM)

-- TODO: run collection errors to array (CoveredFSM)

-- TODO: covered FSM: stop at first error?
