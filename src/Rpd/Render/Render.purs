module Rpd.Render
    ( Renderer(..)
    , RenderF
    , PushMsg(..)
    , Message(..)
    , once
    , run
    , run'
    , make
    , make'
    , update -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API as R
import Rpd.Def as R
import Rpd.Path as R
import Rpd.Command as C
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R


type Message d = C.Command d


data PushMsg d = PushMsg (Message d -> Effect Unit)
type RenderF d r = PushMsg d -> Either R.RpdError (R.Network d) -> r


data Renderer d r
    = Renderer
        r -- initial view
        (RenderF d r)


extractRpd :: forall d r. RenderF d r -> PushMsg d -> R.Rpd (R.Network d) -> Effect r
extractRpd handler pushMsg =
    R.run onError onSuccess
    where
        onError err = handler pushMsg $ Left err
        onSuccess res = handler pushMsg $ Right res


{- render once -}
once :: forall d r. Renderer d r -> R.Rpd (R.Network d) -> Effect r
once (Renderer _ handleResult) =
    extractRpd handleResult neverPush
    where
        neverPush = PushMsg $ const $ pure unit


{- Prepare the rendering cycle with internal message producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.
-}
make
    :: forall d r
     . R.Network d
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
     . { event :: Event (Message d)
       , push :: (Message d -> Effect Unit)
       }
    -> R.Network d
    -> Renderer d r
    -> { first :: r
       , next :: Event (Effect r)
       }
make' { event : messages, push : pushMessage } nw (Renderer initialView handler) =
    let
        updateFlow = Event.fold updater messages $ pure nw
        viewFlow = viewer (PushMsg pushMessage) <$> updateFlow
    in
        { first : initialView
        , next : viewFlow
        }
    where
        updater :: Message d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater msg rpd = rpd >>= update msg
        viewer :: PushMsg d -> R.Rpd (R.Network d) -> Effect r
        viewer pushMessage = extractRpd handler pushMessage


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d r
     . R.Network d
    -> Renderer d r
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
    :: forall d r
     . { event :: Event (Message d)
       , push :: (Message d -> Effect Unit)
       }
    -> R.Network d
    -> Renderer d r
    -> Effect R.Canceler
run' event nw renderer =
    case make' event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)


update :: forall d. Message d -> R.Network d -> R.Rpd (R.Network d)
update (C.AddPatch patchDef) = R.addPatch' patchDef
update (C.AddNode patchId nodeDef) = R.addNode' patchId nodeDef
update _ = pure


