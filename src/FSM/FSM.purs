module FSM
    ( FSM
    , prepare -- FIXME: do not expose
    , make, makeWithPush, makePassing
    , run, run', run'', fold, fold'
    , pushAll, noSubscription
    , imapModel, imapAction
    , AndThen, doNothing, single, batch
    , joinWith

    , join, join', join''
    , foldUpdate, appendEffects, (<:>)
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Console as Console

import Data.List (List, (:))
import Data.List as List
import Data.Foldable (class Foldable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Traversable (traverse_)
import Data.Bifunctor (bimap)
import Data.Functor.Invariant (class Invariant, imap)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Util (Canceler)


type AndThen action = List action


-- TODO: try changing back to `List (Effect action)`
data FSM action model =
    FSM ((action -> Effect Unit) -> action -> model -> model /\ Effect (AndThen action))


instance invariantFSM :: Invariant (FSM action) where
    imap = imapModel


{-}
class DoNothing action where -- a.k.a. `Monoid`?
    doNothing :: action
-}


{-
class Batch action where -- a.k.a. `Semigroup`?
    batch :: List action -> action
    break :: action -> List action
-}


-- instance effectActionSemigroup :: Batch action => Semigroup (Effect action) where
--     append effectA effectB =
--         \actionA actionB -> batch $ List.Cons actionA $ List.singleton actionB


make
    :: forall action model
     . (action -> model -> model /\ Effect (AndThen action))
    -> FSM action model
make = FSM <<< const


makeWithPush
    :: forall action model
     . ((action -> Effect Unit) -> action -> model -> model /\ Effect (AndThen action))
    -> FSM action model
makeWithPush = FSM


makePassing
    :: forall action model
     . FSM action model
makePassing = FSM (\_ _ m -> m /\ doNothing)


noSubscription :: forall a. a -> Effect Unit
noSubscription = const $ pure unit


doNothing :: forall action. Effect (AndThen action)
doNothing = pure List.Nil


single :: forall action. action -> Effect (AndThen action)
single = pure <<< List.singleton


batch :: forall action. List action -> Effect (AndThen action)
batch = pure


join :: forall action. List (AndThen action) -> AndThen action
join = foldr (<>) List.Nil


join' :: forall action. List (AndThen action) -> Effect (AndThen action)
join' = pure <<< join


join'':: forall action. List (Effect (AndThen action)) -> Effect (AndThen action)
join'' = foldr (<:>) $ pure List.Nil


-- newtype PushF action = PushF (action -> Effect Unit)


-- TODO: add `NestFSM` to support placing actions inside other actions, like we do for GUI


prepare
    :: forall action model
     . FSM action model
    -> model
    -> (model -> Effect Unit) -- FIXME: use `update` itself for that?
    -> (action -> Effect Unit) -- FIXME: use `update` itself for that?
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
prepare (FSM f) init subModels subActions = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (model /\ Effect (AndThen action))) =
            Event.fold
                (\action prev -> f pushAction action $ fst prev)
                actions
                (init /\ doNothing)
        (models :: Event model)
            = fst <$> updates
    stopModelSubscription <- Event.subscribe models subModels
    stopActionSubscription <- Event.subscribe actions subActions
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ eff) -> eff >>= traverse_ pushAction
    pure
        { pushAction
        , stop : stopModelSubscription
              <> stopActionSubscription
              <> stopPerformingEffects
        }


-- TODO: just take the `model` and not a list of actions, and that's it,
-- user may perform the initial actions just by running a separate FSM
run
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> f action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm init = do
    run' fsm init noSubscription


run'
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> f action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run' fsm init subscription = do
    run'' fsm init subscription noSubscription


run''
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> (action -> Effect Unit)
    -> f action
    -> Effect
            { pushAction :: action -> Effect Unit
            , stop :: Canceler
            }
run'' fsm init subModels subActions actionList = do
    { pushAction, stop } <- prepare fsm init subModels subActions
    _ <- traverse_ pushAction actionList
    pure { pushAction, stop : stop }



fold
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> f action
    -> Effect
            (model /\
                { pushAction :: action -> Effect Unit
                 -- FIXME: not a lot of sense in returning `pushAction` here
                 ---       and may be `stop` as well
                , stop :: Canceler
            })
    -- -> Effect (model /\ Canceler)
fold fsm init actionList = do
    lastValRef <- Ref.new init
    { pushAction, stop } <- prepare fsm init (flip Ref.write lastValRef) noSubscription
    _ <- traverse_ pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ { pushAction, stop }
    -- fold' fsm init (const $ pure unit) actionList


fold'
    :: forall action model
     . FSM action model
    -> model
    -> (model -> Effect Unit)
    -> List action
    -> Effect (model /\ Canceler)
fold' fsm init subscription actionList = do
    lastValRef <- Ref.new init
    { pushAction, stop } <- prepare fsm init (\model -> do
        _ <- lastValRef # Ref.write model
        _ <- subscription model
        pure unit)
        noSubscription
    _ <- traverse_ pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ stop


pushAll :: forall action. (action -> Effect Unit) -> List action -> Effect Unit
pushAll = traverse_


imapAction
    :: forall actionA actionB model
     . (actionA -> actionB)
    -> (actionB -> actionA)
    -> FSM actionA model
    -> FSM actionB model
imapAction mapAToB mapBToA (FSM updateF) =
    FSM \push actionB model ->
        map (map (map mapAToB))
            $ updateF (mapAToB >>> push) (mapBToA actionB) model


imapModel
    :: forall action modelA modelB
     . (modelA -> modelB)
    -> (modelB -> modelA)
    -> FSM action modelA
    -> FSM action modelB
imapModel mapAToB mapBToA (FSM updateF) =
    FSM \push action modelB ->
        bimap mapAToB identity $ updateF push action $ mapBToA modelB


-- TODO: try to get rid of those
foldUpdate
    :: forall action model
     . (action -> model ->  model /\ Effect (AndThen action))
    -- => FSM action model
    -> model
    -> ( action /\ action )
    -> model /\ Effect (AndThen action)
foldUpdate updateF model ( actionA /\ actionB ) =
    let
        model' /\ effects' = updateF actionA model
        model'' /\ effects'' = updateF actionB model'
    in
        model'' /\ (effects' <:> effects'')


infixr 5 appendEffects as <:>


appendEffects
    :: forall action
     . Effect (AndThen action)
    -> Effect (AndThen action)
    -> Effect (AndThen action)
appendEffects effectA effectB =
    append <$> effectA <*> effectB


joinWith
    :: forall action model
     . (model -> model -> model)
    -> FSM action model
    -> FSM action model
joinWith joinF (FSM updateF) =
    FSM $ \push action model ->
            let model' /\ effects' = updateF push action model
            in (model `joinF` model') /\ effects'
