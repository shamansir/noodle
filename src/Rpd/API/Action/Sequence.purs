module Rpd.API.Action.Sequence where

import Prelude

import Effect (Effect)
import Data.Array (snoc)
import Data.Either
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


newtype ActionList d c n = ActionList (Array (Action d c n))
newtype EveryStep d c n = EveryStep (Network d c n -> Effect Unit)
newtype EveryStep' d c n = EveryStep' (Either RpdError (Network d c n) -> Effect Unit)
newtype ErrorHandler = ErrorHandler (RpdError -> Effect Unit)
newtype LastStep d c n = LastStep (Network d c n -> Effect Unit)
newtype EveryAction d c n = EveryAction (Action d c n -> Effect Unit)


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = ActionList []


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


addNode :: forall d c n. Path.ToPatch -> Path.Alias -> n -> Action d c n
addNode patch alias n = Request $ ToAddNode patch alias n


addInlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addInlet node alias c = Request $ ToAddInlet node alias c


addOutlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addOutlet node alias c = Request $ ToAddOutlet node alias c


pass :: forall d c n. EveryStep d c n
pass = EveryStep $ const $ pure unit


prepare_
    :: forall model action effect
     . model
    -> (action -> model -> Either RpdError (model /\ Array effect))
    -> ((action -> Effect Unit) -> effect -> model -> Effect Unit)
    -> Effect
        { models :: Event (Either RpdError model)
        , actions :: Event action
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
    pure { models, pushAction, stop, actions }


prepare
    :: forall d c n
     . Network d c n
    -> Toolkit d c n
    -> Effect
        { models :: Event (Either RpdError (Network d c n))
        , actions :: Event (Action d c n)
        , pushAction :: Action d c n -> Effect Unit
        , stop :: Effect Unit
        }
prepare nw toolkit = prepare_ nw (apply toolkit) (performEffect toolkit)


run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ErrorHandler
    -> EveryStep d c n
    -> ActionList d c n
    -> Effect Unit
run toolkit initialNW (ErrorHandler onError) (EveryStep everyStep) (ActionList actionList) = do
    { models, pushAction } <- prepare initialNW toolkit
    _ <- Event.subscribe models $ either onError everyStep
    _ <- traverse_ pushAction actionList
    pure unit


run'
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> EveryStep' d c n
    -> ActionList d c n
    -> Effect Unit
run' toolkit initialNW (EveryStep' everyStep) (ActionList actionList) = do
    { models, pushAction } <- prepare initialNW toolkit
    _ <- Event.subscribe models everyStep
    _ <- traverse_ pushAction actionList
    pure unit


run_
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ErrorHandler
    -> LastStep d c n
    -> ActionList d c n
    -> Effect Unit
run_ toolkit initialNW (ErrorHandler onError) (LastStep lastStep) (ActionList actionList) = do
    { models, pushAction } <- prepare initialNW toolkit
    _ <- Event.subscribe models $ either onError $ const $ pure unit
    _ <- traverse_ pushAction actionList
    pushAction $ Inner $ Do lastStep
    pure unit


runTracing
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ErrorHandler
    -> EveryAction d c n
    -> ActionList d c n
    -> Effect Unit
runTracing toolkit initialNW (ErrorHandler onError) (EveryAction everyAction) (ActionList actionList) = do
    { pushAction, actions, models } <- prepare initialNW toolkit
    _ <- Event.subscribe models $ either onError $ const $ pure unit
    _ <- Event.subscribe actions everyAction
    _ <- traverse_ pushAction actionList
    pure unit


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen (ActionList arr) msg = ActionList (arr `snoc` msg)
