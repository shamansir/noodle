module TensorFlow.Toolkit.Render.Html where


import Prelude

import Data.Array.NonEmpty (toArray) as NE
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Noodle.Network (Node(..)) as R
import Noodle.Path as P
import Noodle.Process (Receive, Send) as R
import Noodle.Render.Html (ToolkitRenderer, View) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Spork.Html as H

import TensorFlow.Toolkit.Channel (Channel(..))
import TensorFlow.Toolkit.Node (Node(..), nodesForTheList)
import TensorFlow.Toolkit.Value (Value(..))
import TensorFlow.Toolkit.Render.Html.ToHtml (View, toInlet)


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

renderNode BangNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick $ H.always_ $ toInlet path "bang" $ Bang ]
            [ H.span [ H.classes [ "xodus-connect-button" ] ] [ H.text "◌" ] ]
        ]

renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList

renderNode AddNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]

renderNode LayerNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]




