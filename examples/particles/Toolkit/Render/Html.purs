module Example.Toolkit.Render.Html where

import Prelude

import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (Either(..))
import Data.Number (fromString) as Number
import Data.Array (catMaybes, length, mapWithIndex) as Array
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Data.Spread (run) as Spread
import Data.Spread (Spread) as S
import Data.Vec2 (Vec2(..))
import Data.Vec2 (arrow) as Vec2

-- import Rpd.Toolkit (ToolkitRenderer)
import Rpd.Network as R
import Rpd.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Rpd.API.Action.Sequence as A
import Rpd.Render.Html (View, ToolkitRenderer, core) as R
import Rpd.Render.Html.NodeList (render) as NodeList
import Rpd.Path as P
import Rpd.Process as R

import Rpd.Render.Atom as R

import Spork.Html (Html)
import Spork.Html as H

import FRP.Event as E
import FRP.Event.Time as E
import FRP.Event.AnimationFrame as E

import Example.Toolkit.Nodes
import Example.Toolkit.Value
import Example.Toolkit.Channel
import Example.Toolkit.Html.OnHtml


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
    , renderInlet : \c _ d ->
        H.div
            [ H.classes [ "tk-inlet", classFor c ] ]
            [ H.text $ maybe "?" show d ]
    , renderOutlet : \c _ d ->
        H.div
            [ H.classes [ "tk-outlet", classFor c ] ]
            [ H.text $ maybe "?" show d ]
    }
    where
        classFor TriggerChannel = "tk-trigger"
        classFor NumericalChannel = "tk-number"
        classFor SpreadChannel = "tk-spread"
        classFor AnyValueChannel = "tk-any"
        classFor _ = "tk-any"


renderNode
    :: Node
    -> R.Node Value Node
    -> R.Receive Value
    -> R.Send Value
    -> R.View Value Channel Node
renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList
renderNode BangNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick
                $ H.always_ $ R.core
                $ A.Request
                $ A.ToSendToInlet (P.inletInNode path "bang")
                $ Bang
            ]
            [ H.text "◌" ]
        ]
renderNode TimeNode (R.Node _ path _ _ _) _ _ =
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
renderNode CanvasNode _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.canvas
            [ H.id_ "the-canvas", H.width 300, H.height 300 ]
        ]
renderNode NumberNode (R.Node _ path _ _ _) lastAtInlet _ =
    H.div
        [ H.classes [ "tk-node" ], H.style "display: flex; flex-direction: column;" ]
        [ H.input
            [ H.type_ H.InputNumber
            , H.onValueChange \v ->
                Number.fromString v
                    <#> R.core
                    <<< A.Request
                    <<< A.ToSendToInlet (P.inletInNode path "num")
                    <<< Numerical
            ]
        , H.text (lastAtInlet "num" # maybe "?" show)
        ]
renderNode ColorNode _ lastAtInlet lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ renderIfColor $ lastAtInlet "r" <#> rNumToColor >>= identity <#> Color
        , H.text "+"
        , renderIfColor $ lastAtInlet "g" <#> gNumToColor >>= identity <#> Color
        , H.text "+"
        , renderIfColor $ lastAtInlet "b" <#> bNumToColor >>= identity <#> Color
        , H.text "="
        , renderIfColor $ lastAtOutlet "color"
        ]
    where
        rNumToColor (Numerical r) = Just $ RgbaColor { r, g : 0.0, b : 0.0, a : 1.0 }
        rNumToColor _ = Nothing
        gNumToColor (Numerical g) = Just $ RgbaColor { r : 0.0, g, b : 0.0, a : 1.0 }
        gNumToColor _ = Nothing
        bNumToColor (Numerical b) = Just $ RgbaColor { r : 0.0, g : 0.0, b, a : 1.0 }
        bNumToColor _ = Nothing
renderNode VectorNode _ _ lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case lastAtOutlet "vector" of
            Just (Vector vec2) ->
                H.div
                    []
                    [ renderCoords vec2
                    , renderDirection vec2
                    ]
            _ -> H.div [] []
        ]
    where
        rNumToColor (Numerical r) = Just $ RgbaColor { r, g : 0.0, b : 0.0, a : 1.0 }
        rNumToColor _ = Nothing
        gNumToColor (Numerical g) = Just $ RgbaColor { r : 0.0, g, b : 0.0, a : 1.0 }
        gNumToColor _ = Nothing
        bNumToColor (Numerical b) = Just $ RgbaColor { r : 0.0, g : 0.0, b, a : 1.0 }
        bNumToColor _ = Nothing
renderNode ShapeNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick
                $ H.always_ $ R.core
                $ A.Request
                $ A.ToSendToInlet (P.inletInNode path "shape")
                $ Apply $ Draw $ Ellipse $ Vec2 20.0 20.0
            ]
            [ H.text "CIRCLE" ]
        ]
renderNode SpreadNode _ lastAtInlet lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ renderIfSetAnd isSpread "(None)" $ lastAtOutlet "join"
        ]
renderNode JoinNode _ _ lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ renderIfSetAnd isSpread "(None)" $ lastAtOutlet "join"
        ]
renderNode MoveNode _ _ lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ renderIfSet "(None)" $ lastAtOutlet "move"
        ]
renderNode FillNode _ _ lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ renderIfSet "(None)" $ lastAtOutlet "fill"
        ]
renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]
