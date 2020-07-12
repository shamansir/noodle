module Noodle.Render.Toolkit where


import Data.Maybe (Maybe)
import Noodle.Network (Node, Inlet, Outlet) as R
import Noodle.Process (Receive, Send)


newtype RendererAlias = RendererAlias String


type Renderer d c n view =
    -- FIXME: now any node, inlet and outlet are containing the corresponding `n` or 'c' instance
    --        so it's a duplication to use them as a separate parameters in these functions
    { renderNode :: n -> R.Node d n -> Receive d -> Send d -> view
    -- , renderInlet :: ChannelDefAlias -> R.Inlet d -> c -> (msg -> Effect Unit) -> view
    , renderInlet :: c -> R.Inlet d c -> Maybe d -> view
    -- , renderOutlet :: ChannelDefAlias -> R.Outlet d -> c -> (msg -> Effect Unit) -> view
    , renderOutlet :: c -> R.Outlet d c -> Maybe d -> view
    }
