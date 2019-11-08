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

-- import Rpd.Toolkit (ToolkitRenderer)
import Rpd.Network as R
import Rpd.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Rpd.API.Action.Sequence as A
import Rpd.Renderer.Html (View, ToolkitRenderer, core) as R
import Rpd.Renderer.Html.NodeList (render) as NodeList
import Rpd.Path as P
import Rpd.Process as R

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
        , H.text (lastAtInlet "num" # maybe "?" show)
        ]
renderNode ShapeNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick
                $ H.always_ $ R.core
                $ A.Request
                $ A.ToSendToInlet (P.inletInNode path "shape")
                $ Apply $ Draw $ Ellipse 100.0 100.0
            ]
            [ H.text "CIRCLE" ]
        ]
renderNode SpreadNode (R.Node uuid path _ _ _) lastAtInlet lastAtOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case lastAtOutlet "spread" of
            Just (Spread spread) ->
                renderSpread spread
            _ ->
                H.text "Empty"
        ]
renderNode PairNode (R.Node uuid path _ _ _) lastAtInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            []
            [ H.text $ "spread1" <> (show $ lastAtInlet "spread1")
            , H.text $ "spread2" <> (show $ lastAtInlet "spread2")
            ]
        ]
renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text "tk-node (no renderer)" ]


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
                # H.div []
        aLotOfItems ->
            Array.mapWithIndex (\idx item ->
                if idx `mod` (floor (toNumber len / toNumber maxItems)) == 0 then
                    Just $ idx /\ item
                else Nothing
            ) arr
                # Array.catMaybes
                # map (uncurry renderItem)
                # H.div []
    where
        renderItem _ _ = H.div [] []

