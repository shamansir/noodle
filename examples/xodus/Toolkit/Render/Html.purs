module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (toUnfoldable, List(..), (:), head, zip, mapWithIndex, singleton)
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
               Just (Databases databases) -> H.ul [] $ renderDatabase <$> toUnfoldable databases
               _ -> H.text "no databases"
        ]
    where
        renderDatabase :: Database -> View
        renderDatabase database@(Database { location }) =
            H.li
                [ H.classes [ "xodus-list-item xodus-database" ]
                , H.onClick $ H.always_ $ toInlet path "only" $ SelectDatabase database
                ]
                [ H.text location ]

renderNode AllOfNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atInlet "source" of
                Just (Source database entityTypes) ->
                    H.div
                        []
                        [ render path entityTypes ]
                _ -> H.text "no entity types"
        ]

renderNode SelectNode _ _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atOutlet "result" of
                Just (Result entities) ->
                    H.ul [] $ renderEntity <$> toUnfoldable entities
                _ -> H.text "no entities"
        ]
    where
        renderEntity :: Entity -> View
        renderEntity (Entity entity) =
            H.li
                [ H.classes [ "xodus-list-item xodus-entity" ]
                ]
                [ H.text $ show entity.id ]

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


instance renderEntityTypes :: Render (List EntityType) where
    render path entityTypes =
        grid'
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
                ) entityTypes
            )


-- TODO: abstract over view and path
class Render a where
    render :: P.ToNode -> a -> View


instance renderString :: Render String where
    render _ = H.text


instance renderInt :: Render Int where
    render _ = H.text <<< show


{-
class (Render x rowId, Render x colId) <= ToGrid x rowId colId a where
    cell :: rowId /\ Int -> colId /\ Int -> a -> Html x
-}


grid
    :: forall rowId colId a b
     . Render rowId => Render colId
    => P.ToNode
    -> List colId
    -> (rowId -> a -> List b)
    -> (rowId -> colId -> b -> View)
    -> List (rowId /\ a /\ Maybe Action)
    -> View
grid p = grid' p Nothing


grid'
    :: forall rowId colId a b
     . Render rowId => Render colId
    => P.ToNode
    -> Maybe Action
    -> List colId
    -> (rowId -> a -> List b)
    -> (rowId -> colId -> b -> View)
    -> List (rowId /\ a /\ Maybe Action)
    -> View
grid' nodePath maybeAll headers getCells renderCell rows =
    H.table
        [ H.classes [ "xodus-table" ] ]
            (headerHtml (toUnfoldable headers)
                <> (rowHtml <$> toUnfoldable cells)
                <> (toUnfoldable <$> singleton <$> footerHtml <$> maybeAll # fromMaybe []))
    where
        headerHtml [] = []
        headerHtml headers' =
            [ H.thead [] [ H.tr [] (headerCell <$> headers') ] ]
        headerCell v = H.th [] [ render nodePath v ]
        cells =
                (\(rowId /\ action /\ row) ->
                    rowId /\ action /\ (cellToHtml <$> zip row headers))
            <$> (\(rowId /\ v /\ action) ->
                    rowId /\ action /\ ((/\) rowId <$> getCells rowId v))
            <$> rows
        cellToHtml ((rowId /\ b) /\ colId) = renderCell rowId colId b
        rowHtml (rowId /\ maybeAction /\ row) =
            H.tr
                (case maybeAction of
                    Just action ->
                        [ H.classes
                             [ "xodus-clickable" ]
                        , H.onClick $ H.always_ $ action
                        ]
                    Nothing -> []
                )
                (dataCell <$> toUnfoldable row)
        dataCell v = H.td [] [ v ]
        footerHtml allAction =
            H.tfoot
                (case maybeAll of
                    Just action ->
                        [ H.classes
                             [ "xodus-clickable" ]
                        , H.onClick $ H.always_ $ action
                        ]
                    Nothing -> []
                )
                [ H.tr [] [ H.th [] [ H.text "All" ] ] ]
