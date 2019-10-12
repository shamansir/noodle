module Example.Toolkit.Render.Html where

import Prelude (const, ($), (<>), show, (<<<), map)

import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))

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
                $ map (Time <<< _.time)
                $ E.withTime
                $ E.interval 1
            ]
            [ H.text "SEND" ]
         ]
renderNode CanvasNode (R.Node uuid path _ _ _) =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.canvas [ H.id_ "the-man-canvas", H.width 300, H.height 300 ] ]
renderNode ButtonsNode _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text "tk-node"
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToAddPatch "test" ]
            [ H.text "ADD PATCH" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addNode (P.toPatch "test") "nodelist0" NodeListNode ]
            [ H.text "ADD NODELIST" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addNode (P.toPatch "test") "random1" RandomNode ]
            [ H.text "ADD RANDOM1 NODE" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addNode (P.toPatch "test") "random2" RandomNode ]
            [ H.text "ADD RANDOM2 NODE" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addInlet (P.toNode "test" "random1") "test" NumericChannel ]
            [ H.text "ADD INLET TEST TO RANDOM1" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addInlet (P.toNode "test" "random1") "foo" NumericChannel ]
            [ H.text "ADD INLET FOO TO RANDOM1" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addOutlet (P.toNode "test" "random1") "test" NumericChannel ]
            [ H.text "ADD OUTLET TEST TO RANDOM1" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addOutlet (P.toNode "test" "random1") "foo" NumericChannel ]
            [ H.text "ADD OUTLET FOO TO RANDOM1" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.addInlet (P.toNode "test" "random2") "test" NumericChannel ]
            [ H.text "ADD INLET TEST TO RANDOM2" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random" "min") $ Numeric 10.0 ]
            [ H.text "SEND DATA TO RANDOM/MIN" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "min") $ Numeric 20.0 ]
            [ H.text "SEND DATA TO RANDOM1/MIN" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "test") $ Shape Diamond ]
            [ H.text "SEND DATA TO RANDOM1/TEST" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "foo") $ Shape Diamond ]
            [ H.text "SEND DATA TO RANDOM1/FOO" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random2" "test") $ Shape Diamond ]
            [ H.text "SEND DATA TO RANDOM2/TEST" ]
        , H.div
            [ H.onClick $ H.always_ $ R.core
                $ A.Request
                $ A.ToSendPeriodicallyToInlet (P.toInlet "test" "random1" "max") 1
                $ Numeric <<< toNumber
            ]
            [ H.text "SEND DATA TO RANDOM1/MAX PERIOD" ]
        ]
renderNode _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text "tk-node" ]

