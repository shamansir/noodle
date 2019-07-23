module Rpd.API.Action.Sequence where

import Prelude

import Effect (Effect)
import Data.Array (snoc)
import Data.Either
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Network
import Rpd.API (RpdError)
import Rpd.API.Action
import Rpd.API.Action.Apply (Step, apply, performEffect)
import Rpd.Path as Path
import Rpd.Toolkit (Toolkit)


data ActionList d c n = ActionList (Array (Action d c n))
data EveryStep d c n = EveryStep (Either RpdError (Network d c n) -> Effect Unit)
data LastStep d c n = LastStep (Network d c n -> Effect Unit)


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = ActionList []


none :: forall d c n. ActionList d c n
none = init


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


addNode :: forall d c n. Path.ToPatch -> Path.Alias -> n -> Action d c n
addNode patch alias n = Request $ ToAddNode patch alias n


addInlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addInlet node alias c = Request $ ToAddInlet node alias c


addOutlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addOutlet node alias c = Request $ ToAddOutlet node alias c


prepare_
    :: forall model action effect
     . model
    -> (action -> model -> Either RpdError (model /\ Array effect))
    -> ((action -> Effect Unit) -> effect -> model -> Effect Unit)
    -> Effect
        { models :: Event (Either RpdError model)
        , pushAction :: action -> Effect Unit
        , stop :: Effect Unit
        }
prepare_ initialModel apply performEff = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (Either RpdError (model /\ Array effect))) =
            Event.fold
                (\action step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> apply action model
                )
                actions
                (pure $ initialModel /\ [])
        (models :: Event (Either RpdError model))
            = ((<$>) fst) <$> updates
    stop <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> performEff pushAction eff model) effects
    pure { models, pushAction, stop }


prepare
    :: forall d c n
     . Network d c n
    -> Toolkit d c n
    -> Effect
        { models :: Event (Either RpdError (Network d c n))
        , pushAction :: Action d c n -> Effect Unit
        , stop :: Effect Unit
        }
prepare nw toolkit = prepare_ nw (apply toolkit) (performEffect toolkit)


run_
    :: forall model action effect
     . model
    -> (action -> model -> Either RpdError (model /\ Array effect))
    -> ((action -> Effect Unit) -> effect -> model -> Effect Unit)
    -> Array action
    -> (Either RpdError model -> Effect Unit)
    -> Effect
        { models :: Event (Either RpdError model)
        , pushAction :: action -> Effect Unit
        , stop :: Effect Unit
        }
run_ model apply performEff actions everyStep = do
    { models, pushAction, stop } <- prepare_ model apply performEff
    cancelSubscription <- Event.subscribe models everyStep
    _ <- traverse_ pushAction actions
    pure { models, pushAction, stop : stop <> cancelSubscription }


run__
    :: forall model action effect
     . model
    -> (action -> model -> Either RpdError (model /\ Array effect))
    -> ((action -> Effect Unit) -> effect -> model -> Effect Unit)
    -> Array action
    -> Effect
        { models :: Event (Either RpdError model)
        , pushAction :: action -> Effect Unit
        , stop :: Effect Unit
        }
run__ model apply performEff actions = do
    { models, pushAction, stop } <- prepare_ model apply performEff
    _ <- traverse_ pushAction actions
    pure { models, pushAction, stop }


run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> EveryStep d c n
    -> Effect
            { models :: Event (Either RpdError (Network d c n))
            , pushAction :: (Action d c n) -> Effect Unit
            , stop :: Effect Unit
            }
run toolkit initialNW (ActionList actionList) (EveryStep sub) =
    run_
        initialNW
        (apply toolkit)
        (performEffect toolkit)
        actionList
        sub


run'
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> LastStep d c n
    -> Effect
            { models :: Event (Either RpdError (Network d c n))
            , pushAction :: (Action d c n) -> Effect Unit
            , stop :: Effect Unit
            }
run' toolkit initialNW (ActionList actionList) (LastStep lastStep) = do
    run__
        initialNW
        (apply toolkit)
        (performEffect toolkit)
        (actionList `snoc` (Inner $ Do lastStep))


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen (ActionList arr) msg = ActionList (arr `snoc` msg)
