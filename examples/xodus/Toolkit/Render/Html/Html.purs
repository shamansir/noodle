module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Tuple.Nested ((/\))
import Data.Array.NonEmpty (toArray) as NE

import Noodle.Network (Node(..)) as R
import Noodle.Render.Html (ToolkitRenderer) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Noodle.Path as P
import Noodle.Process (Receive, Send) as R

import Spork.Html as H

import Xodus.Dto (Database, Entity, EntityType, groupByType)
import Xodus.Query (showV) as Q
import Xodus.QueryParser as QParser

import Xodus.Toolkit.Node (Node(..), nodesForTheList)
import Xodus.Toolkit.Value (Value(..), Aggregate(..))
import Xodus.Toolkit.Channel (Channel(..))
import Xodus.Toolkit.Render.Html.ToHtml (View, toHtml, toInlet)
import Xodus.Toolkit.Render.Html.Grid (Grid(..))


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

renderNode TakeNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.span [ H.classes [ "xodus-node-label" ] ] [ H.text "take" ]
        , H.button
            [ H.classes [ "xodus-button" ]
            , H.onClick $ H.always_ $ toInlet path "amount" $ Amount All
            ]
            [ H.text "All" ]
        , H.input
            [ H.classes [ "xodus-number" ]
            , H.type_ H.InputNumber
            , H.value $ case atInlet "amount" of
                Just (Amount (Exactly n)) -> show n
                _ -> "-"
            , H.onValueChange
                (\v ->
                    Int.fromString v
                        <#> toInlet path "amount" <<< Amount <<< Exactly
                )
            ]
        ]

renderNode DropNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.span [ H.classes [ "xodus-node-label" ] ] [ H.text "drop" ]
        , H.button
            [ H.classes [ "xodus-button" ]
            , H.onClick $ H.always_ $ toInlet path "amount" $ Amount All
            ]
            [ H.text "All" ]
        , H.input
            [ H.classes [ "xodus-number" ]
            , H.type_ H.InputNumber
            , H.value $ case atInlet "amount" of
                Just (Amount (Exactly n)) -> show n
                _ -> "-"
            , H.onValueChange
                (\v ->
                    Int.fromString v
                        <#> toInlet path "amount" <<< Amount <<< Exactly
                )
            ]
        ]

renderNode UnionNode (R.Node _ path _ _ _) _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text $ case atOutlet "query" of
            Just (Query query) -> show query
            _ -> "-"
        ]

renderNode IntersectNode (R.Node _ path _ _ _) _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.text $ case atOutlet "query" of
            Just (Query query) -> show query
            _ -> "-"
        ]

renderNode FilterNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.span [ H.classes [ "xodus-node-label" ] ] [ H.text "filter" ]
        , H.input
            [ H.classes [ "xodus-text" ]
            , H.type_ H.InputText
            , H.value $ case atInlet "filter" of
                Just (ToFilter _ info) -> Q.showV info
                _ -> ""
            , H.onValueChange $
                -- FIXME: run parser in the node itself
                QParser.run QParser.condition
                    >>> either (const Nothing)
                        \(info /\ condition) ->
                            Just $ toInlet path "filter" $ ToFilter condition info
            ]
        , H.span [ H.classes [ "xodus-node-val-descr" ] ]
            [ H.text $ case atInlet "filter" of
                Just (ToFilter _ info) -> show info
                _ -> ""
            ]
        ]

renderNode SortNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.span [ H.classes [ "xodus-node-label" ] ] [ H.text "sort" ]
        , H.input
            [ H.classes [ "xodus-text" ]
            , H.type_ H.InputText
            , H.value $ case atInlet "sort" of
                Just (ToSort _ info) -> Q.showV info
                _ -> ""
            , H.onValueChange $
                -- FIXME: run parser in the node itself
                QParser.run QParser.comparison
                    >>> either (const Nothing)
                        \(info /\ comparison) ->
                            Just $ toInlet path "sort" $ ToSort comparison info
            ]
        , H.span [ H.classes [ "xodus-node-val-descr" ] ]
            [ H.text $ case atInlet "sort" of
                Just (ToSort _ info) -> show info
                _ -> ""
            ]
        ]

renderNode SelectNode (R.Node _ path _ _ _) atInlet atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.classes [ "xodus-float" ] ]
            [ H.text "group"
            , H.input
                [ H.classes [ "xodus-check" ]
                , H.type_ H.InputCheckbox
                , H.onChecked $ Just <<< toInlet path "group" <<< Switch
                ]
            ]
        , case atOutlet "result" /\ atInlet "group" of
            Just (Result entities)
                /\ Just (Switch true) ->
                    H.div [] $ renderGroup <$> groupByType entities
            Just (Result entities)
                /\ _ ->
                toHtml path $ Grid entities
            _ ->
                toHtml path $ Grid ([] :: Array Entity)
        ]
    where
        renderGroup (label /\ items) =
            H.div
                [ H.classes [ "xodus-group" ] ]
                [ H.span [ H.classes [ "xodus-group-label" ] ] [ H.text label ]
                , toHtml path $ Grid $ NE.toArray items
                ]
