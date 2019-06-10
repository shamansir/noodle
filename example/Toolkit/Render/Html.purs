module Example.Toolkit.Render.Html where

import Prelude (const)

import Data.Map as Map
import Data.Maybe (Maybe(..))

import Rpd.Toolkit (ToolkitRenderer)
import Rpd.Command (Command)
import Rpd.Renderer.Html (View, Message)

import Spork.Html (Html)
import Spork.Html as H

import Example.Toolkit.Value
import Example.Toolkit.Channel

-- type RenderNode d msg view = forall msg. R.Node d -> (msg -> Effect Unit) -> view
-- type RenderInlet c d msg view = Channels d c => (R.Inlet d -> c -> (msg -> Effect Unit) -> view)
-- type RenderOutlet c d msg view = Channels d c => (R.Outlet d -> c -> (msg -> Effect Unit) -> view)


-- data Renderer msg d c view = Renderer
--     { node :: NodeDefAlias /-> RenderNode msg d view
--     , inlet :: ChannelDefAlias /-> RenderInlet c msg d view
--     , outlet :: ChannelDefAlias /-> RenderOutlet c msg d view
--     }


renderer :: ToolkitRenderer Value Channel (View Value) Message
renderer =
    { renderNode : \_ _ _ -> H.div [] []
    , renderInlet : \_ _ _ _ -> H.div [] []
    , renderOutlet : \_ _ _ _ -> H.div [] []
    }
