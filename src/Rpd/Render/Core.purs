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
import Rpd.Toolkit (Toolkit) as T


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


{- Prepare the rendering cycle with the internal command producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe`, in this case,
   returns the canceler, so it is possible to stop the thing.
-}
make
    :: forall d c view
     . T.Toolkit d c
    -> R.Rpd (R.Network d)
    -> Renderer d view
    -> Effect
        { first :: view
        , next :: Event (Effect view)
        }
make toolkit rpd renderer =
    Event.create >>=
        \event -> pure $ make' event toolkit rpd renderer


{- Prepare the rendering cycle with the custom command producer
   (so, the `Event` with the commands source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe`, in this case,
   returns the canceler, so it is possible to stop the process.

   TODO: do not ask user for `event`, just pushing function.
-}
make'
    :: forall d c view
     . { event :: Event (C.Command d)
       , push :: (C.Command d -> Effect Unit)
       }
    -> T.Toolkit d c
    -> R.Rpd (R.Network d)
    -> Renderer d view
    -> { first :: view
       , next :: Event (Effect view)
       }
make'
    { event : commands, push : pushCommand }
    toolkit
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
        updater cmd rpd' = rpd' >>= C.apply cmd pushCommand toolkit
        viewer :: R.Rpd (R.Network d) -> Effect view
        viewer = extractRpd handler (PushCmd pushCommand)


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d c view
     . T.Toolkit d c
    -> R.Rpd (R.Network d)
    -> Renderer d view
    -> Effect R.Canceler
run toolkit rpd renderer =
    make toolkit rpd renderer >>=
        \{ first, next } -> Event.subscribe next (pure <<< identity)


{- Run the rendering cycle with custom command producer
   (so, the `Event` with the commands source and
   the function which pushes them to this flow).
   Returns the canceler.

   TODO: do not ask user for `event`, just pushing function.
-}
run'
    :: forall d c view
     . { event :: Event (C.Command d)
       , push :: (C.Command d -> Effect Unit)
       }
    -> T.Toolkit d c
    -> R.Rpd (R.Network d)
    -> Renderer d view
    -> Effect R.Canceler
run' toolkit event nw renderer =
    case make' toolkit event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


