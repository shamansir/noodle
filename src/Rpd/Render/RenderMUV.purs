module Rpd.RenderMUV
    ( Renderer(..)
    , UpdateF
    , ViewF
    , Message(..)
    , PushMsg
    , once
    , run
    , run'
    , make
    , make'
    , core
    , custom
    ) where

import Prelude

-- import Type.Eval.Function (type ($))

import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr)

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API (Rpd, RpdError) as R
import Rpd.Render (Message, update) as Core
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R


data Message d msg
    = Core (Core.Message d)
    | Custom msg
data PushMsg d msg = PushMsg (Message d msg -> Effect Unit)
{- UpdateF:
   - gets message: either core one from Rpd.Render, or the custom one used by user in the MUV loop;
   - gets the latest MUV model paired with the latest network state;
   - and returns new MUV model with an array of messages to execute in the next loop, when needed;
-}
type UpdateF d model msg
    =  Message d msg
    -> (model /\ R.Network d)
    -> (model /\ Array (Message d msg))
{- ViewF:
   - gets the function allowing to push messages to the flow (for use in view handlers);
   - gets the latest MUV model paired with the latest network state;
   - and returns new view built using these states;
-}
type ViewF d model view msg =
    PushMsg d msg -> Either R.RpdError (model /\ R.Network d) -> view


data Renderer d model view msg
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d model msg
        , view :: ViewF d model view msg
        }


core :: forall d m. Core.Message d -> Message d m
core = Core


custom :: forall d m. m -> Message d m
custom = Custom


extractRpd
    :: forall d model view msg
     . ViewF d model view msg
    -> PushMsg d msg
    -> R.Rpd (model /\ R.Network d)
    -> Effect view
extractRpd view pushMsg rpd =
    R.run onError onSuccess rpd
    where
        onError err = view pushMsg $ Left err
        onSuccess res = view pushMsg $ Right res


{- render once -}
once
    :: forall d model view msg
     . Renderer d model view msg
    -> R.Rpd (R.Network d)
    -> Effect view
once (Renderer { view, init, update }) rpd =
    extractRpd view neverPush withModel
    where
        withModel = (/\) init <$> rpd
        neverPush = PushMsg $ const $ pure unit


{- Prepare the rendering cycle with internal message producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.
-}
make
    :: forall d model view msg
     . R.Rpd (R.Network d)
    -> Renderer d model view msg
    -> Effect
        { first :: view
        , next :: Event (Effect view)
        }
make nw renderer =
    Event.create >>=
        \event -> pure $ make' event nw renderer


{- Prepare the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.

   TODO: do not ask user for `event`, just pushing function.
-}
make'
    :: forall d model view msg
     . { event :: Event (Message d msg)
       , push :: (Message d msg -> Effect Unit)
       }
    -> R.Rpd (R.Network d)
    -> Renderer d model view msg
    -> { first :: view
       , next :: Event (Effect view)
       }
make'
    { event : messages, push : pushMessage }
    rpd
    (Renderer { from, init, view, update : update' })
    = let
        updateFlow = Event.fold updater messages $ (/\) init <$> rpd
        viewFlow = viewer (PushMsg pushMessage) <$> updateFlow
    in
        { first : from
        , next : viewFlow
        }
    where
        updater
            :: Message d msg
            -> R.Rpd (model /\ R.Network d)
            -> R.Rpd (model /\ R.Network d)
        updater msg rpd = rpd >>=
            \(model /\ nw) -> do
                nw' <- case msg of
                    Core coreMsg -> Core.update coreMsg nw
                    Custom _ -> pure nw
                let model' /\ msgs = update' msg $ model /\ nw'
                model'' /\ nw'' <-
                    foldr updater (pure $ model' /\ nw') msgs
                pure $ model'' /\ nw''
        viewer
            :: PushMsg d msg
            -> R.Rpd (model /\ R.Network d)
            -> Effect view
        viewer pushMessage =
            extractRpd view pushMessage


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d view model msg
     . R.Rpd (R.Network d)
    -> Renderer d view model msg
    -> Effect R.Canceler
run nw renderer =
    make nw renderer >>=
        \{ first, next } -> Event.subscribe next (pure <<< identity)


{- Run the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   TODO: do not ask user for `event`, just pushing function.
-}
run'
    :: forall d view model msg
     . { event :: Event (Message d msg)
       , push :: (Message d msg -> Effect Unit)
       }
    -> R.Rpd (R.Network d)
    -> Renderer d view model msg
    -> Effect R.Canceler
run' event nw renderer =
    case make' event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


