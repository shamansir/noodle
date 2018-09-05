module Rpd.Render
    ( Renderer(..)
    , PushMsg
    , Message(..)
    , once
    , run
    , run'
    , proxy
    , proxy'
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


proxy
    :: forall d r x
     . (R.RpdError -> x)
    -> (r -> x)
    -> x
    -> Renderer d r
    -> Renderer d x
proxy onError' convertView default' (Renderer default onError onSuccess) =
    Renderer
        default'
        onError'
        (\push nw -> convertView $ onSuccess push nw)


proxy'
    :: forall d r x
     . (R.RpdError -> x)
    -> ((Message d -> Effect Unit) -> r -> x)
    -> x
    -> Renderer d r
    -> Renderer d x
proxy' onError' convertView default' (Renderer default onError onSuccess) =
    Renderer
        default'
        onError'
        (\push@(PushMsg pushF) -> convertView pushF <<< onSuccess push)


{- render once -}
once :: forall d r. Renderer d r -> R.Rpd (R.Network d) -> Effect r
once (Renderer _ onError onSuccess) =
    R.run onError $ onSuccess (PushMsg $ const $ pure unit)


data Message d
    = Bang
    | AddNode (R.NodeDef d)
    | RemoveNode R.NodePath
    | SelectNode R.NodePath


{- run rendering cycle -}
run :: forall d r. R.Network d -> Renderer d r -> Effect r
run nw renderer =
    Event.create >>=
        \{ event : messages, push : pushMessage }
            -> run' messages (PushMsg pushMessage) nw renderer


{- run rendering cycle with custom event producer -}
run'
    :: forall d r
     . Event (Message d)
    -> PushMsg d
    -> R.Network d
    -> Renderer d r
    -> Effect r
run' messages pushMessage nw (Renderer initialView onError onSuccess) = do
    let uiFlow = Event.fold updater messages $ pure nw
    cancel <- Event.subscribe uiFlow $ viewer pushMessage
    pure initialView
    where
        updater :: Message d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater ui rpd = rpd >>= update ui
        viewer :: PushMsg d -> R.Rpd (R.Network d) -> Effect r
        viewer pushMessage = R.run onError $ onSuccess pushMessage


update :: forall d. Message d -> R.Network d -> R.Rpd (R.Network d)
update ui nw = do
    pure nw
