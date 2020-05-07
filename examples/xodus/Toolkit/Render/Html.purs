module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.List (toUnfoldable, List(..), (:), head, zip)
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
    -> R.View Value Channel Node

renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList

renderNode ConnectNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick $ toInlet path "bang" $ Bang ]
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
        renderDatabase :: Database -> R.View Value Channel Node
        renderDatabase database@(Database { location }) =
            H.li
                [ H.classes [ "xodus-list-item xodus-database" ]
                , H.onClick $ toInlet path "only" $ SelectDatabase database
                ]
                [ H.text location ]

renderNode AllOfNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atInlet "source" of
                Just (Source database entityTypes) ->
                    H.div
                        []
                        [ H.span
                            [ H.onClick $ toInlet path "only" $ Bang ]
                            [ H.text "None" ]
                        , H.ul [] $ renderEntityType <$> toUnfoldable entityTypes
                        ]
                _ -> H.text "no entity types"
        ]
    where
        renderEntityType :: EntityType -> R.View Value Channel Node
        renderEntityType entityType@(EntityType { id, name }) =
            H.li
                [ H.classes [ "xodus-list-item xodus-entity-type" ]
                , H.onClick $ toInlet path "only" $ SelectType $ entityType
                ]
                [ H.text $ show id <> ": " <> name ]

renderNode SelectNode _ _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atOutlet "result" of
                Just (Result entities) ->
                    H.ul [] $ renderEntity <$> toUnfoldable entities
                _ -> H.text "no entities"
        ]
    where
        renderEntity :: Entity -> R.View Value Channel Node
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


toInlet :: P.ToNode -> P.Alias -> Value -> _
toInlet path alias v =
    H.always_ $ R.core
        $ A.Request
        $ A.ToSendToInlet (P.inletInNode path alias)
        $ v


toOutlet :: P.ToNode -> P.Alias -> Value -> _
toOutlet path alias v =
    H.always_ $ R.core
        $ A.Request
        $ A.ToSendToOutlet (P.outletInNode path alias)
        $ v


viewGrid
    :: forall a x
     . Show a
    => (a -> Int -> x)
    -> List (List a)
    -> Html x
viewGrid handler =
    viewGrid' handler Nil


viewGrid'
    :: forall a x
     . Show a
    => (a -> Int -> x)
    -> List a
    -> List (List a)
    -> Html x
viewGrid' handler headers cells =
    H.table
        [ H.classes [ "xodus-table" ] ]
        (headerHtml (toUnfoldable headers)
            <> cellsHtml (toUnfoldable <$> toUnfoldable cells))
    where headerCell v = H.th [] [ H.text $ show v ]
          dataCell v = H.td [] [ H.text $ show v ]
          headerHtml [] = []
          headerHtml headers =
                [ H.tr [] (headerCell <$> headers) ]
          cellsHtml [] = []
          cellsHtml cells =
                (\row -> H.tr [] (dataCell <$> row)) <$> cells


class Render x a where
    render :: a -> Html x


instance renderShow :: Show a => Render x a
    where render = H.text <<< show


{-
class (Render x rowId, Render x colId) <= ToGrid x rowId colId a where
    cell :: rowId /\ Int -> colId /\ Int -> a -> Html x
-}


grid
    :: forall action rowId colId a b
     . Render action rowId => Render action colId
    => List colId
    -> (rowId -> colId -> b -> Html action)
    -- -> (rowId /\ Int -> colId /\ Int -> b -> Html x)
    -> (rowId -> a -> List b)
    -> List (rowId /\ a /\ action)
    -> Html action
grid headers renderCell getCells rows = H.table
    [ H.classes [ "xodus-table" ] ]
        (headerHtml (toUnfoldable headers)
            <> (rowHtml <$> toUnfoldable cells))
    where
        headerHtml [] = []
        headerHtml headers' =
            [ H.tr [] (headerCell <$> headers') ]
        headerCell v = H.th [] [ render v ]
        cells :: List (rowId /\ action /\ List (Html action))
        cells =
                (\(rowId /\ action /\ row) ->
                    rowId /\ action /\ (cellToHtml <$> zip row headers))
            <$> (\(rowId /\ v /\ action) ->
                    rowId /\ action /\ ((/\) rowId <$> getCells rowId v))
            <$> rows
        cellToHtml ((rowId /\ b) /\ colId) = renderCell rowId colId b
        rowHtml (rowId /\ action /\ row) =
            H.tr [] (dataCell <$> toUnfoldable row)
        dataCell v = H.td [] [ v ]

{-
gridV
    :: forall x rowId colId a b
     . Render x rowId => Render x colId
    => (rowId /\ Int -> colId /\ Int -> b -> Html x)
    -- -> (rowId -> a -> List (colId /\ b))
    -> (rowId -> a -> List b)
    -> List (rowId /\ a)
    -> Html x
gridV renderCell cells rows = H.table
    [ H.classes [ "xodus-table" ] ]
        (headerHtml (toUnfoldable headers)
            <> cellsHtml (toUnfoldable <$> toUnfoldable cells))
    where
        headers =
            foldr
                (\(rowId /\ a) headers' ->
                    rowId : headers'
                ) Nil rows
        headerCell v = H.th [] [ render v ]
        dataCell v = H.td [] [ H.text $ show v ]
        headerHtml [] = []
        headerHtml headers =
            [ H.tr [] (headerCell <$> headers) ]
        cellsHtml [] = []
        cellsHtml cells =
            (\row -> H.tr [] (dataCell <$> row)) <$> cells
-}
