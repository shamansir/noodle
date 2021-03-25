module TensorFlow.Toolkit.Render.Html where


import Prelude

import Debug.Trace as DT
import Data.Array.NonEmpty (toArray) as NE
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.String as String
import Noodle.Network (Node(..)) as R
import Noodle.Path as P
import Noodle.Process (Receive, Send) as R
import Noodle.Render.Html (ToolkitRenderer, View) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Spork.Html as H
import TensorFlow.Toolkit.Channel (Channel(..))
import TensorFlow.Toolkit.Node (Node(..), nodesForTheList)
import TensorFlow.Toolkit.Render.Html.ToHtml (View, toInlet)
import TensorFlow.Toolkit.Value (Value(..))


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
        classFor Channel = "tk-channel"


renderNode
    :: Node
    -> R.Node Value Node
    -> R.Receive Value
    -> R.Send Value
    -> View

renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList

renderNode AddNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]

renderNode InputLayerNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.input
            [ H.type_ H.InputText
            -- , H.value $ case atInlet "amount" of
            --     Just (Amount (Exactly n)) -> show n
            --     _ -> "-"
            , H.onValueChange
                (\v ->
                    case String.split (String.Pattern ":") v of
                        [ n1str, n2str, n3str ] ->
                            (\n1 n2 n3 ->
                                toInlet path "shape" $ Shape (n1 /\ n2 /\ n3))
                            <$> Int.fromString n1str
                            <*> Int.fromString n2str
                            <*> Int.fromString n3str
                        _ -> Nothing

                    -- Int.fromString v
                    --     <#> toInlet path "shape" <<< ShapeElement1
                )
            ]
        ]


renderNode TfModelNode (R.Node _ path _ _ _) _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case atOutlet "codestr" of
            Just (Code str) -> H.text str
            _ -> H.text "empty"
        ]




