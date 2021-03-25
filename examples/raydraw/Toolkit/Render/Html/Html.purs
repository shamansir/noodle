module RayDraw.Toolkit.Render.Html where


import Prelude

import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Noodle.Network (Node(..)) as R
import Noodle.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Noodle.Path as P

import Noodle.Process (Receive, Send) as R
import Noodle.Render.Html (View, RoutedAction, ToolkitRenderer, core) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Spork.Html as H
import RayDraw.Toolkit.Channel (Channel(..))
import RayDraw.Toolkit.Node (Node(..), nodesForTheList)
import RayDraw.Toolkit.Render.Html.ToHtml (View, toInlet)
import RayDraw.Toolkit.Value (Value(..))
import Noodle.Render.Atom as R


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

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]


