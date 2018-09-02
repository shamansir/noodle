module Rpd.Render.Terminal
    ( TerminalRenderer
    , terminalRenderer
    ) where

import Prelude

import Effect (Effect, foreachE)
import Rpd as R
import Rpd.Render

type TerminalRenderer d = Renderer d String

terminalRenderer :: forall d. TerminalRenderer d
terminalRenderer =
    Renderer renderError renderNetwork


renderNetwork :: forall d. R.Network d -> Effect String
renderNetwork nw =
    pure "SUCC"


renderError :: R.RpdError -> Effect String
renderError err =
    pure "ERR"
