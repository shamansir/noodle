module Rpd.Render.Terminal
    ( TerminalRenderer
    , terminalRenderer
    ) where

import Control.Monad.Free
import Prelude
import Rpd.Render

import Data.Traversable (sequence, traverse)
import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import FRP.Event (Event, filterMap)
import FRP.Event as Event
import Rpd as R
import Unsafe.Coerce (unsafeCoerce)

type TerminalRenderer d = Renderer d String


data UiMessage d
    = AddNode (R.NodeDef d)
    | RemoveNode R.NodePath


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer renderError renderNetwork


renderNetwork :: forall d. R.Network d -> Effect String
renderNetwork nw = do
    { event : messages, push : pushMessage } <- Event.create
    let uiFlow = Event.fold updater messages $ pure nw
    cancel <- Event.subscribe uiFlow viewer
    pure "SUCC"
    where
        updater :: UiMessage d -> R.Rpd (R.Network d) -> R.Rpd (R.Network d)
        updater ui rpd = rpd >>= update ui
        viewer :: R.Rpd (R.Network d) -> Effect String
        viewer = unsafeCoerce


update :: forall d. UiMessage d -> R.Network d -> R.Rpd (R.Network d)
update ui nw = do
    pure nw


view :: forall d. R.Network d -> Effect String
view nw =
    pure "SUCC"


renderError :: R.RpdError -> Effect String
renderError err =
    pure "ERR"
