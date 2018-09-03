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
import Rpd.Render as RR
import Unsafe.Coerce (unsafeCoerce)

type TerminalRenderer d = Renderer d String


terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer reportError view


view :: forall d. (UiMessage d -> Effect Unit) -> R.Network d -> Effect String
view pushMsg nw =
    pure "SUCC"


reportError :: R.RpdError -> Effect String
reportError err =
    pure "ERR"
