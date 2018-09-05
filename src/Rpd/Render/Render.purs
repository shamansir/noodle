module Rpd.Render
    ( Renderer(..)
    , PushMsg
    , UiMessage(..)
    , render
    , runRender
    , runRender'
    , proxy
    , update
    ) where

import Prelude
import Effect (Effect)
import FRP.Event (Event, filterMap)
import FRP.Event as Event
import Rpd as R


type PushMsg d = UiMessage d -> Effect Unit


data Renderer d r
    = Renderer
        r -- initial view
        (R.RpdError -> Effect r)
        (PushMsg d -> R.Network d -> Effect r)
        -- TODO: change to (PushMsg d -> Either R.RpdError (R.Network d) -> Effect r) maybe?


proxy
    :: forall d r x
     . (R.RpdError -> Effect x)
    -> (r -> Effect x)
    -> x
    -> Renderer d r
    -> Renderer d x
proxy onError' convertView default' (Renderer default onError onSuccess) =
    Renderer
        default'
        onError'
        (\push nw -> onSuccess push nw >>= convertView)


{- render once -}
render :: forall d r. Renderer d r -> R.Rpd (R.Network d) -> Effect r
render (Renderer _ onError onSuccess) =
    R.run' onError $ onSuccess (const $ pure unit)


data UiMessage d
    = AddNode (R.NodeDef d)
    | RemoveNode R.NodePath
    | SelectNode R.NodePath


{- run rendering cycle -}
runRender :: forall d r. R.Network d -> Renderer d r -> Effect r
runRender nw renderer =
    Event.create >>=
        \{ event : messages, push : pushMessage }
            -> runRender' messages pushMessage nw renderer

{- run rendering cycle with custom event producer -}
runRender'
    :: forall d r
     . Event (UiMessage d)
    -> PushMsg d
    -> R.Network d
    -> Renderer d r
    -> Effect r
runRender' messages pushMessage nw (Renderer initialView onError onSuccess) = do
    let uiFlow = Event.fold updater messages $ pure nw
    cancel <- Event.subscribe uiFlow $ viewer pushMessage
    pure initialView
    where
        updater :: UiMessage d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater ui rpd = rpd >>= update ui
        viewer :: PushMsg d -> R.Rpd (R.Network d) -> Effect r
        viewer pushMessage = R.run' onError $ onSuccess pushMessage


update :: forall d. UiMessage d -> R.Network d -> R.Rpd (R.Network d)
update ui nw = do
    pure nw
