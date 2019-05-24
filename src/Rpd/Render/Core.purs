module Rpd.Render
    ( Renderer(..)
    , RenderF
    , PushCmd(..)
    , once
    , run
    , run'
    , make
    , make'
    , neverPush
    ) where


import Prelude

import Data.Either (Either(..))

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API (Rpd, RpdError) as R
import Rpd.Command (Command) as C
import Rpd.CommandApply (apply) as C
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R


-- data RendererCommand d rcmd
--     = Core (C.Command d)
--     | Renderer rcmd


-- data PushCmd d rcmd =
--     PushCmd (RendererCommand d rcmd -> Effect Unit)


-- type RenderF d rcmd view
--     =  PushCmd (RendererCommand d rcmd -> Effect Unit)
--     -> Either R.RpdError (R.Network d)
--     -> view

data PushCmd d =
    PushCmd (C.Command d -> Effect Unit)


type RenderF d view
    =  PushCmd d
    -> Either R.RpdError (R.Network d)
    -> view


data Renderer d view
    = Renderer
        view -- initial view
        (RenderF d view)


neverPush :: forall d. PushCmd d
neverPush = PushCmd $ const $ pure unit


extractRpd
    :: forall d view
     . RenderF d view
    -> PushCmd d
    -> R.Rpd (R.Network d)
    -> Effect view
extractRpd handler pushCmd =
    R.run onError onSuccess
    where
        onError err = handler pushCmd $ Left err
        onSuccess res = handler pushCmd $ Right res


{- render once -}
once :: forall d view. Renderer d view -> R.Rpd (R.Network d) -> Effect view
once (Renderer _ handleResult) =
    extractRpd handleResult neverPush


{- Prepare the rendering cycle with internal message producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.
-}
make
    :: forall d r
     . R.Rpd (R.Network d)
    -> Renderer d r
    -> Effect
        { first :: r
        , next :: Event (Effect r)
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
    :: forall d r
     . { event :: Event (C.Command d)
       , push :: (C.Command d -> Effect Unit)
       }
    -> R.Rpd (R.Network d)
    -> Renderer d r
    -> { first :: r
       , next :: Event (Effect r)
       }
make'
    { event : commands, push : pushCommand }
    rpd
    (Renderer initialView handler) =
    let
        updateFlow = Event.fold updater commands rpd
        viewFlow = viewer <$> updateFlow
    in
        { first : initialView
        , next : viewFlow
        }
    where
        updater :: C.Command d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater cmd rpd' = rpd' >>= C.apply cmd pushCommand
        viewer :: R.Rpd (R.Network d) -> Effect r
        viewer = extractRpd handler (PushCmd pushCommand)


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d r
     . R.Rpd (R.Network d)
    -> Renderer d r
    -> Effect R.Canceler
run nw renderer =
    make nw renderer >>=
        \{ first, next } -> Event.subscribe next (pure <<< identity)


{- Run the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the canceler.

   TODO: do not ask user for `event`, just pushing function.
-}
run'
    :: forall d r
     . { event :: Event (C.Command d)
       , push :: (C.Command d -> Effect Unit)
       }
    -> R.Rpd (R.Network d)
    -> Renderer d r
    -> Effect R.Canceler
run' event nw renderer =
    case make' event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


