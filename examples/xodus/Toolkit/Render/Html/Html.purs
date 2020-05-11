module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Array ((:), head, zip, mapWithIndex, singleton, length)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, foldr)

import Noodle.Network (Node(..)) as R
import Noodle.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Noodle.Render.Html (View, ToolkitRenderer, core, my) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Noodle.Path as P
import Noodle.Process (Receive, Send) as R

import Noodle.Render.Atom as R

import Spork.Html (Html)
import Spork.Html as H

import Xodus.Toolkit.Node (Node(..), nodesForTheList)
import Xodus.Toolkit.Value (Value(..))
import Xodus.Toolkit.Channel (Channel(..))
import Xodus.Dto

import Xodus.Toolkit.Render.Html.ToHtml
import Xodus.Toolkit.Render.Html.Grid (grid, grid', Grid(..))


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

renderNode ConnectNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick $ H.always_ $ toInlet path "bang" $ Bang ]
            [ H.span [ H.classes [ "xodus-connect-button" ] ] [ H.text "◌" ] ]
        ]

renderNode SourceNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case atInlet "databases" of
            Just (Databases databases) ->
                toHtml path $ Grid databases
            _ ->
                toHtml path $ Grid ([] :: Array Database)
        ]

renderNode AllOfNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atInlet "source" of
                Just (Source database entityTypes) ->
                    toHtml path $ Grid entityTypes
                _ ->
                    toHtml path $ Grid ([] :: Array EntityType)
        ]

renderNode SelectNode (R.Node _ path _ _ _) _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atOutlet "result" of
                Just (Result entities) ->
                    toHtml path $ Grid entities
                _ ->
                    toHtml path $ Grid ([] :: Array Entity)
        ]

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ ]
            [ ]
        ]
