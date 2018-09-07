module Rpd.Render
    ( Renderer(..)
    , PushMsg
    , Message(..)
    , once
    , run
    , run'
    , make
    , make'
    , update -- TODO: do not expose maybe?
    ) where

import Prelude

import Data.Tuple (uncurry)
import Effect (Effect)
import FRP.Event (Event, filterMap)
import FRP.Event as Event
import Rpd as R


data PushMsg d = PushMsg (Message d -> Effect Unit)


data Renderer d r
    = Renderer
        r -- initial view
        (R.RpdError -> r)
        (PushMsg d -> R.Network d -> r)
        -- TODO: change to (PushMsg d -> Either R.RpdError (R.Network d) -> Effect r) maybe?


{- render once -}
once :: forall d r. Renderer d r -> R.Rpd (R.Network d) -> Effect r
once (Renderer _ onError onSuccess) =
    R.run onError $ onSuccess (PushMsg $ const $ pure unit)


data Message d
    = Bang
    | AddPatch (R.PatchDef d)
    | AddNode R.PatchId (R.NodeDef d)
    | RemoveNode R.NodePath
    | SelectNode R.NodePath


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
make' { event : messages, push : pushMessage } nw (Renderer initialView onError onSuccess) =
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
        viewer pushMessage = R.run onError $ onSuccess pushMessage


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
update (AddPatch patchDef) = R.addPatch' patchDef
update (AddNode patchId nodeDef) = R.addNode' patchId nodeDef
update _ = pure


