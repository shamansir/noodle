module FSM
    ( FSM(..) -- FIXME: do not expose constructor
    , prepare -- FIXME: do not expose
    , make, makePassing
    , run, run', run'', fold
    , pushAll, noSubscription
    , imapModel, imapAction
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Console as Console

import Data.List (List)
import Data.List as List
import Data.Foldable (class Foldable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)
import Data.Traversable (traverse_)
import Data.Bifunctor (bimap)
import Data.Functor.Invariant (class Invariant, imap)


import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Util (Canceler)


data FSM action model =
    -- TODO: try: (action -> model -> Effect (model /\ Array action))
    FSM (action -> model -> model /\ Effect action)
    -- Array -> Foldable & Applicative & Monoid


instance invariantFSM :: Invariant (FSM action) where
    imap = imapModel


make
    :: forall action model
     . (action -> model -> model /\ Effect action)
    -> FSM action model
make = FSM


makePassing
    :: forall action model
     . Monoid action
    => FSM action model
makePassing = FSM (\_ m -> m /\ pure mempty)


noSubscription :: forall a. a -> Effect Unit
noSubscription = const $ pure unit


-- FIXME: change `Monoid` requirement to some custom typeclass (`IsAction`?)
--        since we break monoid laws: `mempty <> action != mempty.
--        maybe to something like `DoNothing` typeclass.


-- TODO: optionally, add `Batch` typeclass to work with returning multiple actions
--       it will require not only to allow joining two (or more?) actions but also
--       `Foldable` or be able to fold itself (Traverse?) using sequential calls
--       to `update`


{-
updateF (Pair actionA actionB) model =
    let
        model' /\ effects' = updateF actionA model
        model'' /\ effects'' = updateF actionB model'
    in
        model'' /\ (effects' <> effects'')
-}


{-
apply toolkit (Join actionA actionB) nw =
    case apply toolkit actionA nw of
        coveredNw /\ effects ->
            let
                coveredNw' /\ effects' = apply toolkit actionB $ Covered.recover coveredNw
            in
                Covered.joinErrors coveredNw coveredNw' /\ (effects <> effects')
-}


-- TODO: add `NestFSM` to support placing actions inside other actions, like we do for GUI


prepare
    :: forall action model
     . Monoid action -- FIXME: we only use `mempty`, not `append`
    => FSM action model
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
        (updates :: Event (model /\ Effect action)) =
            Event.fold
                (\action prev -> f action $ fst prev)
                actions
                (init /\ pure mempty)
        (models :: Event model)
            = fst <$> updates
    stopModelSubscription <- Event.subscribe models subModels
    stopActionSubscription <- Event.subscribe actions subActions
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ eff) -> eff >>= pushAction
    pure
        { pushAction
        , stop : stopModelSubscription
              <> stopActionSubscription
              <> stopPerformingEffects
        }


run
    :: forall action model f
     . Monoid action
    => Foldable f
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
     . Monoid action
    => Foldable f
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
     . Monoid action
    => Foldable f
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
     . Monoid action
    => Foldable f
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


{- fold'
    :: forall action model
     . Monoid action
    => FSM action model
    -> model
    -> (model -> Effect Unit)
    -> List action
    -> Effect (model /\ Canceler)
fold' fsm init subscription actionList = do
    lastValRef <- Ref.new init
    { pushAction, stop } <- prepare fsm init $ \model -> do
        _ <- lastValRef # Ref.write model
        _ <- subscription model
        pure unit
    _ <- traverse_ pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ lastVal /\ stop -}


pushAll :: forall action. (action -> Effect Unit) -> List action -> Effect Unit
pushAll = traverse_


imapAction
    :: forall actionA actionB model
     . (actionA -> actionB)
    -> (actionB -> actionA)
    -> FSM actionA model
    -> FSM actionB model
imapAction mapAToB mapBToA (FSM updateF) =
    FSM \actionB model ->
        map (map mapAToB) $ updateF (mapBToA actionB) model


imapModel
    :: forall action modelA modelB
     . (modelA -> modelB)
    -> (modelB -> modelA)
    -> FSM action modelA
    -> FSM action modelB
imapModel mapAToB mapBToA (FSM updateF) =
    FSM \action modelB ->
        bimap mapAToB identity $ updateF action $ mapBToA modelB
