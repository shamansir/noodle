module Rpd.API.Action.Sequence where

import Prelude

import Effect (Effect)
import Data.Array (snoc)
import Data.Either
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
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


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = ActionList []


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> (Either RpdError (Network d c n) -> Effect Unit)
    -> Effect Unit
run toolkit initialNW (ActionList actionList) sub = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (Step d c n)) =
            Event.fold
                (\action step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> apply toolkit action model)
                actions
                (pure $ initialNW /\ [])
        (models :: Event (Either RpdError (Network d c n)))
            = ((<$>) fst) <$> updates
    stopEffects <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> performEffect toolkit pushAction eff model) effects
    stopSubscriptions <- Event.subscribe models sub
    _ <- traverse_ pushAction actionList
    --pushAction Start
    _ <- stopEffects <> stopSubscriptions
    pure unit


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen (ActionList arr) msg = ActionList (arr `snoc` msg)
