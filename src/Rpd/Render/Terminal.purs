module Rpd.Render.Terminal
    ( TerminalRenderer
    , terminalRenderer
    , view -- TODO: do not expose maybe?
    , reportError
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
    Renderer "" reportError view


view :: forall d. PushMsg d -> R.Network d -> String
view pushMsg nw =
    "SUCC"


reportError :: R.RpdError -> String
reportError err =
    "ERR"
