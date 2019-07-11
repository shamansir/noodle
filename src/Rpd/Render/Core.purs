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

import Rpd.API (RpdError) as R
import Rpd.API.Command (Command) as C
import Rpd.API.CommandApply (apply) as C
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

data PushCmd d c n =
    PushCmd (C.Command d c n -> Effect Unit)


type RenderF d c n view
    =  PushCmd d c n
    -> Either R.RpdError (R.Network d c n)
    -> view


data Renderer d c n view
    = Renderer
        view -- initial view
        (RenderF d c n view)


neverPush :: forall d c n. PushCmd d c n
neverPush = PushCmd $ const $ pure unit


prepare
    :: forall d c n view
     . R.Network d c n
    -> (R.RpdError -> view)
    -> (R.Network d c n -> view)
    -> Effect
            { pushCmd :: C.Command d c n -> Effect Unit
            , views :: Event view
            -- , stop :: Effect Unit
            }
prepare initialNW onError onValue = do
    { event : commands, push : pushCmd } <- Event.create
    { event : views, push : pushView } <- Event.create
    let
        (updates :: Event (Step d c n)) =
            Event.fold
                (\cmd step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> apply cmd model)
                commands
                (pure $ initialNW /\ [])
        (models :: Event (Either RpdError (Network d c n)))
            = ((<$>) fst) <$> updates
    _ <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> performEffect eff pushCmd model) effects
    _ <- Event.subscribe models (pushView <<< either onError onValue)
    -- stopping is not needed since user just could stop pushing commands
    -- with this particular `pushCmd`
    -- let stopAll = stopEffects <> stopViews
    -- pure { pushCmd, views, stop : stopAll }
    pure { pushCmd, views }


extractRpd
    :: forall d c n view
     . RenderF d c n view
    -> PushCmd d c n
    -> R.Rpd (R.Network d c n)
    -> Effect view
extractRpd handler pushCmd =
    R.run onError onSuccess
    where
        onError err = handler pushCmd $ Left err
        onSuccess res = handler pushCmd $ Right res


{- render once -}
once :: forall d c n view. Renderer d c n view -> R.Rpd (R.Network d c n) -> Effect view
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
    :: forall d c n view
     . T.Toolkit d c n
    -> R.Rpd (R.Network d c n)
    -> Renderer d c n view
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
    :: forall d c n view
     . { event :: Event (C.Command d c n)
       , push :: (C.Command d c n -> Effect Unit)
       }
    -> T.Toolkit d c n
    -> R.Rpd (R.Network d c n)
    -> Renderer d c n view
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
        updater :: C.Command d c n -> R.Rpd (R.Network d c n) -> R.Rpd (R.Network d c n)
        updater cmd rpd' = rpd' >>= C.apply cmd pushCommand toolkit
        viewer :: R.Rpd (R.Network d c n) -> Effect view
        viewer = extractRpd handler (PushCmd pushCommand)


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d c n view
     . T.Toolkit d c n
    -> R.Rpd (R.Network d c n)
    -> Renderer d c n view
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
    :: forall d c n view
     . { event :: Event (C.Command d c n)
       , push :: (C.Command d c n -> Effect Unit)
       }
    -> T.Toolkit d c n
    -> R.Rpd (R.Network d c n)
    -> Renderer d c n view
    -> Effect R.Canceler
run' toolkit event nw renderer =
    case make' toolkit event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


