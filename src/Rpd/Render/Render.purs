module Rpd.Render
    ( Renderer(..)
    , PushMsg
    , UiMessage(..)
    , render
    , runRender
    , runRender'
    ) where

import Prelude
import Effect (Effect)
import FRP.Event (Event, filterMap)
import FRP.Event as Event
import Rpd as R


type PushMsg d = UiMessage d -> Effect Unit


data Renderer d r
    = Renderer
        (R.RpdError -> Effect r)
        (PushMsg d -> R.Network d -> Effect r)
        -- TODO: change to (PushMsg d -> Either R.RpdError (R.Network d) -> Effect r) maybe?

{- render once -}
render :: forall d r. Renderer d r -> R.Rpd (R.Network d) -> Effect r
render (Renderer onError onSuccess) =
    R.run' onError $ onSuccess (const $ pure unit)


data UiMessage d
    = AddNode (R.NodeDef d)
    | RemoveNode R.NodePath
    | SelectNode R.NodePath


{- run rendering cycle -}
runRender :: forall d r. R.Network d -> r -> Renderer d r -> Effect r
runRender nw default renderer =
    Event.create >>=
        \{ event : messages, push : pushMessage }
            -> runRender' messages pushMessage nw default renderer


{- run rendering cycle with custom event producer -}
runRender'
    :: forall d r
     . Event (UiMessage d)
    -> PushMsg d
    -> R.Network d
    -> r
    -> Renderer d r
    -> Effect r
runRender' messages pushMessage nw default (Renderer onError onSuccess) = do
    let uiFlow = Event.fold updater messages $ pure nw
    cancel <- Event.subscribe uiFlow $ viewer pushMessage
    pure default
    where
        updater :: UiMessage d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater ui rpd = rpd >>= update ui
        viewer :: PushMsg d -> R.Rpd (R.Network d) -> Effect r
        viewer pushMessage = R.run' onError $ onSuccess pushMessage


update :: forall d. UiMessage d -> R.Network d -> R.Rpd (R.Network d)
update ui nw = do
    pure nw
