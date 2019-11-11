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
import Rpd.Renderer.Html (View, ToolkitRenderer, core) as R
import Rpd.Renderer.Html.NodeList (render) as NodeList
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


renderNode :: Node -> R.Node Value Node -> R.Receive Value -> R.Send Value -> R.View Value Channel Node
renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList
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
            Just (Vector vec2) -> renderVector vec2
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
                $ Apply $ Draw $ Ellipse $ Vec2 100.0 100.0
            ]
            [ H.text "CIRCLE" ]
        ]
renderNode SpreadNode _ lastAtInlet lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case lastAtOutlet "spread" of
            Just (Spread spread) ->
                renderSpread spread
            _ ->
                H.text "(None)"
        ]
renderNode JoinNode _ _ lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            []
            [ case lastAtOutlet "join" of
                Just (Spread spread) ->
                    renderSpread spread
                _ ->
                    H.text "(None)"
            ]
        ]
renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]


renderSpread :: S.Spread Value -> R.View Value Channel Node
renderSpread spread =
    let
        maxItems = 10
        arr = Array.catMaybes $ Spread.run spread
        len = Array.length arr
    in case arr of
        [] -> H.div [] []
        items | len <= maxItems ->
            Array.mapWithIndex (/\) items
                # map (uncurry renderItem)
                # H.div [ H.classes [ "tk-spread-value" ] ]
        aLotOfItems ->
            Array.mapWithIndex (\idx item ->
                if idx `mod` (floor (toNumber len / toNumber maxItems)) == 0 then
                    Just $ idx /\ item
                else Nothing
            ) arr
                # Array.catMaybes
                # map (uncurry renderItem)
                # H.div [ H.classes [ "tk-spread-value" ] ]
    where
        renderItem index (Numerical num) = H.div [] [ H.text $ show index <> ":" <> show num ]
        renderItem index (Color color) =
            H.div []
                [ H.text $ show index <> ":"
                , renderColor color
                ]
        renderItem index (Vector vec) =
            H.div []
                [ H.text $ show index <> ":"
                , renderVector vec
                ]
        renderItem index _ = H.div [] []


renderIfColor :: Maybe Value -> R.View Value Channel Node
renderIfColor (Just (Color color)) = renderColor color
renderIfColor _ = renderNoColor


renderColor :: RgbaColor -> R.View Value Channel Node
renderColor color =
    H.div
        [ H.classes [ "tk-color-value" ]
        , H.style $ "background-color: " <> colorToCss color <> ";"
        ]
        [ ]


renderNoColor :: R.View Value Channel Node
renderNoColor =
    H.div
        [ H.classes [ "tk-color-value" ]
        ]
        [ H.text "?" ]


renderVector :: Vec2 -> R.View Value Channel Node
renderVector vec =
    H.div
        [ H.classes [ "tk-vector-value" ]
        , H.style $ "transform: rotate(" <> (Vec2.arrow (Vec2 0.0 0.0 /\ vec) # _.angle # show) <> "rad);"
        ]
        [ H.text "⟶" ]


{-
getLinkTransformStyle :: LinkTransform -> String
getLinkTransformStyle { from, angle, length } =
    "transform: translate(" <> fromPosStr <> ") rotate(" <> angleStr <> ");"
        <> " width: " <> lengthStr <> ";"
    where
        fromPosStr = show from.x <> "px, " <> show from.y <> "px"
        angleStr = show angle <> "rad"
        lengthStr = show length <> "px"
-}
