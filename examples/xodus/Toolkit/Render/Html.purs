module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (toUnfoldable, List(..), (:), head, zip, mapWithIndex, singleton, length)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, foldr)

import Noodle.Network (Node(..)) as R
import Noodle.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Noodle.Render.Html (View, ToolkitRenderer, core, my, RoutedAction) as R
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


type View = R.View Value Channel Node


type Action = R.RoutedAction Value Channel Node


class Render a where
    render :: P.ToNode -> a -> View


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
                render path databases
            _ -> render path (Nil :: List Database)
        ]

renderNode AllOfNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atInlet "source" of
                Just (Source database entityTypes) ->
                    render path entityTypes
                _ -> render path (Nil :: List EntityType)
        ]

renderNode SelectNode (R.Node _ path _ _ _) _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atOutlet "result" of
                Just (Result entities) ->
                    render path entities
                _ -> render path (Nil :: List Entity)
        ]

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ ]
            [ ]
        ]


toInlet :: P.ToNode -> P.Alias -> Value -> Action
toInlet path alias v =
    R.core
        $ A.Request
        $ A.ToSendToInlet (P.inletInNode path alias)
        $ v


toOutlet :: P.ToNode -> P.Alias -> Value -> Action
toOutlet path alias v =
    R.core
        $ A.Request
        $ A.ToSendToOutlet (P.outletInNode path alias)
        $ v


instance renderDatbases :: Render (List Database) where
    render path entityTypes =
        grid
            Nil
            path
            (singleton "location")
            (\_ (Database { location }) -> singleton location)
            (\_ _ name -> H.text name)
            (mapWithIndex
                (\index db ->
                    index
                        /\ db
                        /\ (Just $ toInlet path "only" $ SelectDatabase $ db)
                ) entityTypes
            )


instance renderEntityTypes :: Render (List EntityType) where
    render path databases =
        grid'
            Nil
            path
            (Just $ toInlet path "only" Bang)
            (singleton "name")
            (\_ (EntityType { name }) -> singleton name)
            (\_ _ name -> H.text name)
            (mapWithIndex
                (\index etype ->
                    index
                        /\ etype
                        /\ (Just $ toInlet path "only" $ SelectType $ etype)
                ) databases
            )


instance renderEntities :: Render (List Entity) where
    render path entities =
        grid
            (singleton "xodus-wide-ticker")
            path
            (singleton "type")
            (\_ (Entity { type : type_ }) -> singleton type_ )
            (\_ _ prop -> H.text prop)
            (mapWithIndex
                (\_ entity@(Entity { id }) ->
                    id
                        /\ entity
                        /\ Nothing
                ) entities
            )


instance renderString :: Render String where
    render _ = H.text


instance renderInt :: Render Int where
    render _ = H.text <<< show


grid
    :: forall rowId colId a b
     . Render rowId => Render colId
    => List String
    -> P.ToNode
    -> List colId
    -> (rowId -> a -> List b)
    -> (rowId -> colId -> b -> View)
    -> List (rowId /\ a /\ Maybe Action)
    -> View
grid classes p = grid' classes p Nothing


grid'
    :: forall rowId colId a b
     . Render rowId => Render colId
    => List String
    -> P.ToNode
    -> Maybe Action
    -> List colId
    -> (rowId -> a -> List b)
    -> (rowId -> colId -> b -> View)
    -> List (rowId /\ a /\ Maybe Action)
    -> View
grid' classes nodePath maybeAll headers getCells renderCell Nil =
    H.table
        [ H.classes ([ "xodus-table" ] <> toUnfoldable classes) ]
        [ H.tr [] [ H.td [] [ H.text "Empty" ] ] ]
grid' classes nodePath maybeAll headers getCells renderCell rows =
    H.table
        [ H.classes ([ "xodus-table" ] <> toUnfoldable classes) ]
            (headerHtml (toUnfoldable $ mapWithIndex ((/\)) headers)
                <> (rowHtml <$> (toUnfoldable $ mapWithIndex ((/\)) cells) # wrapRows)
                <> (toUnfoldable
                        <$> singleton
                        <$> footerHtml (length rows)
                        <$> maybeAll # fromMaybe []))
    where
        ordClasses count idx | idx == 0 && idx == (count - 1) = [ "xodus-first", "xodus-last" ]
        ordClasses count idx | idx == 0 = [ "xodus-first"]
        ordClasses count idx | idx == (count - 1) = [ "xodus-last" ]
        ordClasses count idx | otherwise = []
        headerHtml [] = []
        headerHtml headers' =
            [ H.thead [] [ H.tr [] ([ emptyTicker ] <> (headerCell <$> headers')) ] ]
        headerCell (colIdx /\ colId) =
            H.th
                [ H.classes $ ordClasses (length headers) colIdx ]
                [ render nodePath colId ]
        wrapRows r =
            [ H.tbody [] r ]
        cells =
                (\(rowId /\ action /\ row) ->
                    rowId /\ action /\ (cellToHtml <$> zip row headers))
            <$> (\(rowId /\ v /\ action) ->
                    rowId /\ action /\ ((/\) rowId <$> getCells rowId v))
            <$> rows
        cellToHtml ((rowId /\ b) /\ colId) = renderCell rowId colId b
        ticker rowId = H.td [ H.classes [ "xodus-ticker" ] ] [ render nodePath rowId ]
        emptyTicker = H.th [ H.classes [ "xodus-ticker" ] ] [ H.text "ID" ]
        rowHtml (rowIdx /\ (rowId /\ maybeAction /\ row)) =
            H.tr
                (case maybeAction of
                    Just action ->
                        [ H.classes
                            ([ "xodus-clickable" ] <> ordClasses (length rows) rowIdx)
                        , H.onClick $ H.always_ $ action
                        ]
                    Nothing ->
                        [ H.classes $ ordClasses (length rows) rowIdx ]
                )
                $ [ ticker rowId ]
                    <> (dataCell (length row) <$> (toUnfoldable $ mapWithIndex ((/\)) row))
        dataCell cellCount (cellIdx /\ v) =
            H.td [ H.classes $ ordClasses cellCount cellIdx ] [ v ]
        footerHtml colCount allAction =
            H.tfoot
                (case maybeAll of
                    Just action ->
                        [ H.classes
                             [ "xodus-clickable" ]
                        , H.onClick $ H.always_ $ action
                        ]
                    Nothing -> []
                )
                [ H.tr
                    [ H.classes [ "xodus-first", "xodus-last" ] ]
                    ([ emptyTicker ]
                        <> [ H.th
                                [ H.colSpan colCount
                                , H.classes [ "xodus-first", "xodus-last" ]
                                ]
                                [ H.text "All" ]
                            ])
                    ]
