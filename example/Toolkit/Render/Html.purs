module Example.Toolkit.Render.Html where

import Data.Map as Map

import Rpd.Toolkit (Renderer(..))
import Rpd.Command (Command)
import Rpd.Renderer.Html (View)

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


renderer :: Renderer Value Channel (Command Value) (View Value)
renderer =
    Renderer
        { node : Map.empty
        , inlet : Map.empty
        , outlet : Map.empty
        }
