module Rpd.API.Action.Sequence where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Ref as Ref

import Control.Monad.Error.Class
import Control.Alternative ((<|>))

import Data.List (List(..), snoc)
import Data.Either
import Data.Maybe
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Network
import Rpd.API
    ( RpdError
    , NodeInletsSubscription, NodeOutletsSubscription
    , InletSubscription, OutletSubscription
    )
import Rpd.API.Action
import Rpd.API.Action.Apply (Step, apply, performEffect)
import Rpd.Path as Path
import Rpd.Toolkit (Toolkit, NodeDef)
import Rpd.Util (Canceler)


type ActionList d c n = List (Action d c n) -- TODO: or newtype?
type EveryStep d c n = Either RpdError (Network d c n) -> Effect Unit
type EveryAction d c n = Action d c n -> Effect Unit
type FoldResult d c n = Either RpdError (Network d c n)
type BasicContinuationResult model action =
    { models :: Event (Either RpdError model)
    , actions :: Event action
    , pushAction :: action -> Effect Unit
    , stop :: Canceler
    }
type ContinuationResult d c n =
    BasicContinuationResult (Network d c n) (Action d c n)


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = Nil


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


addNode :: forall d c n. Path.ToPatch -> Path.Alias -> n -> Action d c n
addNode patch alias n = Request $ ToAddNode patch alias n


addNextNode :: forall d c n. Path.ToPatch -> n -> Action d c n
addNextNode patch n = Request $ ToAddNextNode patch n


addNodeByDef :: forall d c n. Path.ToPatch -> Path.Alias -> n -> NodeDef d c -> Action d c n
addNodeByDef patch alias n def = Request $ ToAddNodeByDef patch alias n def


addNextNodeByDef :: forall d c n. Path.ToPatch -> n -> NodeDef d c -> Action d c n
addNextNodeByDef patch n def = Request $ ToAddNextNodeByDef patch n def


removeNode :: forall d c n. Path.ToNode -> Action d c n
removeNode = Request <<< ToRemoveNode


addInlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addInlet node alias c = Request $ ToAddInlet node alias c


addOutlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addOutlet node alias c = Request $ ToAddOutlet node alias c


removeInlet :: forall d c n. Path.ToInlet -> Action d c n
removeInlet inlet = Request $ ToRemoveInlet inlet


removeOutlet :: forall d c n. Path.ToOutlet -> Action d c n
removeOutlet outlet = Request $ ToRemoveOutlet outlet


connect :: forall d c n. Path.ToOutlet -> Path.ToInlet -> Action d c n
connect outlet inlet = Request $ ToConnect outlet inlet


disconnect :: forall d c n. Path.ToOutlet -> Path.ToInlet -> Action d c n
disconnect outlet inlet = Request $ ToDisconnect outlet inlet


sendToInlet :: forall d c n. Path.ToInlet -> d -> Action d c n
sendToInlet inlet d = Request $ ToSendToInlet inlet d


sendToOutlet :: forall d c n. Path.ToOutlet -> d -> Action d c n
sendToOutlet outlet d = Request $ ToSendToOutlet outlet d


streamToInlet :: forall d c n. Path.ToInlet -> (Event d) -> Action d c n
streamToInlet inlet event = Request $ ToStreamToInlet inlet event


streamToOutlet :: forall d c n. Path.ToOutlet -> (Event d) -> Action d c n
streamToOutlet outlet event = Request $ ToStreamToOutlet outlet event


subscribeToInlet :: forall d c n. Path.ToInlet -> InletSubscription d -> Action d c n
subscribeToInlet inlet handler = Request $ ToSubscribeToInlet inlet handler


subscribeToOutlet :: forall d c n. Path.ToOutlet -> OutletSubscription d -> Action d c n
subscribeToOutlet outlet handler = Request $ ToSubscribeToOutlet outlet handler


subscribeToNode
    :: forall d c n
     . Path.ToNode
    -> NodeInletsSubscription d
    -> NodeOutletsSubscription d
    -> Action d c n
subscribeToNode node inletsHandler outletsHandler =
    Request $ ToSubscribeToNode node inletsHandler outletsHandler


do_ :: forall d c n. (Perform d c n) -> Action d c n
do_ f = Inner $ Do f


pass :: forall d c n. EveryStep d c n
pass = const $ pure unit


-- FIXME: Do not expose!
prepare_
    :: forall model action effect
     . model
    -> (action -> model -> Either RpdError (model /\ Array effect))
    -> ((action -> Effect Unit) -> effect -> model -> Effect Unit)
    -> Effect (BasicContinuationResult model action)
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
    pure { models, pushAction, actions, stop }


prepare
    :: forall d c n
     . Network d c n
    -> Toolkit d c n
    -> Effect (ContinuationResult d c n)
prepare nw toolkit = prepare_ nw (apply toolkit) (performEffect toolkit)


{-
run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> EveryStep d c n
    -> ActionList d c n
    -> Effect Unit
run toolkit initialNW stepHandler actions =
    run' toolkit initialNW stepHandler actions
        *> pure unit
-}


run
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> EveryStep d c n
    -> ActionList d c n
    -> Effect (ContinuationResult d c n)
run toolkit initialNW stepHandler actions = do
    res@{ models, pushAction, stop } <- prepare initialNW toolkit
    lastValRef <- Ref.new $ Right initialNW
    stopInforming <- Event.subscribe models stepHandler
    _ <- pushAll pushAction actions
    -- _ <- stopInforming
    pure res { stop = stop <> stopInforming }


runFolding
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> Effect (FoldResult d c n /\ ContinuationResult d c n)
runFolding toolkit initialNW actions = do
    res@{ models, pushAction, stop } <- prepare initialNW toolkit
    lastValRef <- Ref.new $ Right initialNW
    let modelsFolded = Event.fold (<|>) models $ Right initialNW
    stopCollectingLastValue <-
        Event.subscribe modelsFolded (flip Ref.write lastValRef)
    _ <- pushAll pushAction actions
    -- _ <- stopCollectingLastValue
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ res { stop = stop <> stopCollectingLastValue }


{-
TODO: implement, so could work with `Aff` and replace `failOrGet` in RpdTest.Helper
TODO: Think about laziness, so that if some error occurs,
      skip execution of the tail of the action list

runM
    :: forall m d c n
     . MonadError RpdError m
    => MonadEffect m
    => Toolkit d c n
    -> Network d c n
    -> ActionList d c n
    -> m (Network d c n)
runM toolkit initialNW actions =
    liftEffect $ do
        { models, pushAction } <- liftEffect $ prepare initialNW toolkit
        lastValRef <- Ref.new $ Right initialNW
        stopInforming <- Event.subscribe models ?wh
        stopCollectingLastValue <- Event.subscribe models (flip Ref.write lastValRef)
        _ <- pushAll pushAction actions
        -- _ <- stopInforming
        -- _ <- stopCollectingLastValue
        lastVal <- Ref.read lastValRef
        pure $ initialNW
-}


runTracing
    :: forall d c n
     . Toolkit d c n
    -> Network d c n
    -> EveryAction d c n
    -> ActionList d c n
    -> Effect (FoldResult d c n /\ ContinuationResult d c n)
runTracing toolkit initialNW everyAction actionList = do
    res@{ models, pushAction, actions, stop } <- prepare initialNW toolkit
    lastValRef <- Ref.new $ Right initialNW
    let modelsFolded = Event.fold (<|>) models $ Right initialNW
    stopCollectingLastValue <-
        Event.subscribe modelsFolded (flip Ref.write lastValRef)
    stopListeningActions <-
        Event.subscribe actions everyAction
    _ <- pushAll pushAction actionList
    -- _ <- stopInforming
    -- _ <- stopCollectingLastValue
    -- _ <- stopListeningActions
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ res { stop = stop <> stopCollectingLastValue <> stopListeningActions }


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen = snoc


pushAll :: forall d c n. (Action d c n -> Effect Unit) -> ActionList d c n -> Effect Unit
pushAll = traverse_
