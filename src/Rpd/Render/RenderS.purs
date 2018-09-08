module Rpd.RenderS
    ( Renderer(..)
    , UpdateF
    , ViewF
    , once
    , run
    , run'
    , make
    , make'
    ) where

import Prelude

import Data.Either
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API as R
import Rpd.Def as R
import Rpd.Path as R
import Rpd.Render (PushMsg(..), Message(..), update)
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R


type UpdateF d model = Message d -> model -> R.Network d -> model
type ViewF d view model = PushMsg d -> Either R.RpdError (model /\ R.Network d) -> view


data Renderer d view model
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d model
        , view :: ViewF d view model
        }


extractRpd
    :: forall d view model
     . ViewF d view model
    -> PushMsg d
    -> R.Rpd (model /\ R.Network d)
    -> Effect view
extractRpd view pushMsg rpd =
    R.run onError onSuccess rpd
    where
        onError err = view pushMsg $ Left err
        onSuccess res = view pushMsg $ Right res


{- render once -}
once :: forall d view model. Renderer d view model -> R.Rpd (R.Network d) -> Effect view
once (Renderer { view, init }) rpd =
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
    :: forall d view model
     . R.Network d
    -> Renderer d view model
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
    :: forall d view model
     . { event :: Event (Message d)
       , push :: (Message d -> Effect Unit)
       }
    -> R.Network d
    -> Renderer d view model
    -> { first :: view
       , next :: Event (Effect view)
       }
make'
    { event : messages, push : pushMessage }
    nw
    (Renderer { from, init, view, update : update' })
    = let
        updateFlow = Event.fold updater messages $ pure (init /\ nw)
        viewFlow = viewer (PushMsg pushMessage) <$> updateFlow
    in
        { first : from
        , next : viewFlow
        }
    where
        updater :: Message d -> R.Rpd (model /\ R.Network d) -> R.Rpd (model /\ R.Network d)
        updater msg rpd = rpd >>=
            \(model /\ nw) -> do
                nw <- update msg nw
                let model' = update' msg model nw
                pure $ model' /\ nw
        viewer :: PushMsg d -> R.Rpd (model /\ R.Network d) -> Effect view
        viewer pushMessage = extractRpd view pushMessage


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d view model
     . R.Network d
    -> Renderer d view model
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
    :: forall d view model
     . { event :: Event (Message d)
       , push :: (Message d -> Effect Unit)
       }
    -> R.Network d
    -> Renderer d view model
    -> Effect R.Canceler
run' event nw renderer =
    case make' event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


