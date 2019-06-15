module Example.Toolkit.Render.Html where

import Prelude (const, ($))

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- import Rpd.Toolkit (ToolkitRenderer)
import Rpd.Command (Command(..)) as C
import Rpd.Renderer.Html (View, Message, ToolkitRenderer, core) as R
import Rpd.Path as P

import Spork.Html (Html)
import Spork.Html as H

import Example.Toolkit.Nodes
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


-- type Command = R.Command Value Channel Node
-- type View = R.View Value Channel Node


renderer :: R.ToolkitRenderer Value Channel Node
renderer =
    { renderNode : \_ _ _ ->
        H.div
            [ H.classes [ "tk-node" ] ]
            [ H.text "tk-node"
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ C.AddInlet (P.toNode "test" "random") "test" ColorChannel ]
                [ H.text "ADD INLET" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ C.SendToInlet (P.toInlet "test" "random" "min") $ Shape Cross ]
                [ H.text "SEND DATA" ]
            ]
    , renderInlet : \_ _ _ -> H.div [ H.classes [ "tk-inlet" ] ] [ H.text "tk-inlet" ]
    , renderOutlet : \_ _ _ -> H.div [ H.classes [ "tk-outlet" ] ] [ H.text "tk-outlet" ]
    }
