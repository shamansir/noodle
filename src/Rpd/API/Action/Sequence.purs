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


data ActionList d c n = ActionList (Array (Action d c n))
data EveryStep d c n = EveryStep (Either RpdError (Network d c n) -> Effect Unit)
data LastStep d c n = LastStep (Network d c n -> Effect Unit)


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = ActionList []


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


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


run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> EveryStep d c n
    -> Effect Unit
run toolkit initialNW (ActionList actionList) (EveryStep sub) = do
    { models, pushAction } <- prepare initialNW toolkit
    _ <- Event.subscribe models sub
    _ <- traverse_ pushAction actionList
    pure unit


run'
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> LastStep d c n
    -> Effect Unit
run' toolkit initialNW (ActionList actionList) (LastStep lastStep) = do
    { models, pushAction } <- prepare initialNW toolkit
    _ <- traverse_ pushAction actionList
    pushAction $ Inner $ Do lastStep
    pure unit


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen (ActionList arr) msg = ActionList (arr `snoc` msg)
