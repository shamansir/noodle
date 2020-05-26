module FSM
    ( FSM
    , make, makeWithPush, makePassing
    , makeMinimal, makeWithNoEffects
    , run, run', run'', run_, fold, fold'
    , pushAll, noSubscription
    , update, update'
    , imapModel, imapAction
    , doNothing, single, batch, just
    , joinWith
    , foldUpdate
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref

import Data.List (List, (:))
import Data.List as List
import Data.Foldable (class Foldable, foldr)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)
import Data.Bifunctor (bimap)
import Data.Functor.Invariant (class Invariant, imap)

import FRP.Event (Event)
import FRP.Event as Event

import Noodle.Util (Canceler)


data FSM action model =
    FSM ((action -> Effect Unit) -> action -> model -> model /\ List (Effect action))


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
     . (action -> model -> model /\ List (Effect action))
    -> FSM action model
make = FSM <<< const


makeWithPush
    :: forall action model
     . ((action -> Effect Unit) -> action -> model -> model /\ List (Effect action))
    -> FSM action model
makeWithPush = FSM


makePassing
    :: forall action model
     . FSM action model
makePassing = FSM (\_ _ m -> m /\ doNothing)


makeMinimal
    :: forall action model
     . (action -> model -> model)
    -> FSM action model
makeMinimal updateF =
    FSM \_ action model -> updateF action model /\ doNothing


makeWithNoEffects
    :: forall action model
     . (action -> model -> model /\ List action)
    -> FSM action model
makeWithNoEffects updateF =
    FSM \_ action model -> map pure <$> updateF action model


noSubscription :: forall a. a -> Effect Unit
noSubscription = const $ pure unit


doNothing :: forall action. List (Effect action)
doNothing = List.Nil


just :: forall action. Effect action -> List (Effect action)
just = List.singleton


single :: forall action. action -> List (Effect action)
single = pure >>> just


batch :: forall action. List action -> List (Effect action)
batch = (<$>) pure



-- newtype PushF action = PushF (action -> Effect Unit)


-- TODO: add `NestFSM` to support placing actions inside other actions, like we do for GUI


-- TODO: just take the `model` and not a list of actions, and that's it,
-- user may perform the initial actions just by running a separate FSM
run
    :: forall action model
     . FSM action model
    -> model
    -> Effect
            { push :: action -> Effect Unit
            , stop :: Canceler
            }
run fsm init = do
    run' fsm init noSubscription


run'
    :: forall action model
     . FSM action model
    -> model
    -> (model -> Effect Unit)
    -> Effect
            { push :: action -> Effect Unit
            , stop :: Canceler
            }
run' fsm init subscription = do
    run'' fsm init subscription noSubscription


run_
    :: forall action model
     . FSM action model
    -> model
    -> (action -> Effect Unit)
    -> Effect
            { push :: action -> Effect Unit
            , stop :: Canceler
            }
run_ fsm init =
    run'' fsm init noSubscription


run''
    :: forall action model
     . FSM action model
    -> model
    -> (model -> Effect Unit)
    -> (action -> Effect Unit)
    -> Effect
            { push :: action -> Effect Unit
            , stop :: Canceler
            }
run'' (FSM f) init subModels subActions = do
    { event : actions, push : push } <- Event.create
    let
        (updates :: Event (model /\ List (Effect action))) =
            Event.fold
                (\action prev -> f push action $ fst prev)
                actions
                (init /\ doNothing)
        (models :: Event model)
            = fst <$> updates
    stopModelSubscription <- Event.subscribe models subModels
    stopActionSubscription <- Event.subscribe actions subActions
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ effs) -> traverse_ ((=<<) push) effs
    pure
        { push
        , stop : stopModelSubscription
               <> stopActionSubscription
               <> stopPerformingEffects
        }


fold
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> f action
    -> Effect
            (model /\
                { push :: action -> Effect Unit
                 -- FIXME: not a lot of sense in returning `push` here
                 ---       and may be `stop` as well
                , stop :: Canceler
            })
    -- -> Effect (model /\ Canceler)
fold fsm init actionList = do
    lastValRef <- Ref.new init
    { push, stop } <- run'' fsm init (flip Ref.write lastValRef) noSubscription
    _ <- traverse_ push actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ { push, stop }
    -- fold' fsm init (const $ pure unit) actionList


fold'
    :: forall action model f
     . Foldable f
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> f action
    -> Effect (model /\ Canceler)
fold' fsm init subscription actionList = do
    lastValRef <- Ref.new init
    { push, stop } <- run'' fsm init (\model -> do
        _ <- lastValRef # Ref.write model
        _ <- subscription model
        pure unit)
        noSubscription
    _ <- traverse_ push actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ stop


pushAll
    :: forall action f
     . Foldable f
    => (action -> Effect Unit)
    -> f action
    -> Effect Unit
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
     . (action -> model ->  model /\ List (Effect action))
    -- => FSM action model
    -> model
    -> ( action /\ action )
    -> model /\ List (Effect action)
foldUpdate updateF model ( actionA /\ actionB ) =
    let
        model' /\ effects' = updateF actionA model
        model'' /\ effects'' = updateF actionB model'
    in
        model'' /\ (effects' <> effects'')


joinWith
    :: forall action model
     . (model -> model -> model)
    -> FSM action model
    -> FSM action model
joinWith joinF (FSM updateF) =
    FSM $ \push action model ->
            let model' /\ effects' = updateF push action model
            in (model `joinF` model') /\ effects'


update
    :: forall action model
     . FSM action model
    -> (action -> Effect Unit)
    -> action
    -> model
    -> model /\ List (Effect action)
update (FSM updateF) = updateF


update'
    :: forall action model
     . FSM action model
    -> action
    -> model
    -> model /\ List (Effect action)
update' (FSM updateF) = updateF $ const $ pure unit
