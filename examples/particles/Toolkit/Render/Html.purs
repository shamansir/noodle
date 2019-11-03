module Example.Toolkit.Render.Html where

import Prelude

import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Number (fromString) as Number

-- import Rpd.Toolkit (ToolkitRenderer)
import Rpd.Network as R
import Rpd.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Rpd.API.Action.Sequence as A
import Rpd.Renderer.Html (View, ToolkitRenderer, core) as R
import Rpd.Renderer.Html.NodeList (render) as NodeList
import Rpd.Path as P

import Spork.Html (Html)
import Spork.Html as H

import FRP.Event as E
import FRP.Event.Time as E
import FRP.Event.AnimationFrame as E

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
    { renderNode : renderNode
    , renderInlet : \_ _ d ->
        H.div
            [ H.classes [ "tk-inlet" ] ]
            [ H.text $ "tk-inlet : " <> (maybe "?" show d) ]
    , renderOutlet : \_ _ d ->
        H.div
            [ H.classes [ "tk-outlet" ] ]
            [ H.text $ "tk-outlet : " <> (maybe "?" show d) ]
    }




renderNode :: Node -> R.Node Value Node -> R.View Value Channel Node
renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) =
    NodeList.render (P.ToPatch patch) nodesForTheList
renderNode TimeNode (R.Node uuid path _ _ _) =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request
                $ A.ToStreamToInlet (P.inletInNode path "time")
                $ map (const Bang)
                $ E.animationFrame
            ]
            [ H.text "SEND" ]
         ]
renderNode CanvasNode (R.Node uuid path _ _ _) =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.canvas
            [ H.id_ "the-canvas", H.width 300, H.height 300 ]
        ]
renderNode NumberNode (R.Node uuid path _ _ _) =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.input
            [ H.type_ H.InputNumber
            , H.onValueChange \v ->
                Number.fromString v
                    <#> R.core
                    <<< A.Request
                    <<< A.ToSendToInlet (P.inletInNode path "num")
                    <<< Numerical
            ]
        ]
renderNode _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text "tk-node (no renderer)" ]

