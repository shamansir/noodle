module Xodus.Toolkit.Render.Html.Grid where


import Prelude

import Data.List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))

import Spork.Html as H

import Noodle.Path as P

import Xodus.Dto
import Xodus.Toolkit.Value (Value(..))
import Xodus.Toolkit.Render.Html.ToHtml


newtype Grid a = Grid a


instance toHtmlDatbases :: ToHtml (Grid (List Database)) where
    toHtml path (Grid entityTypes) =
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


instance toHtmlEntityTypes :: ToHtml (Grid (List EntityType)) where
    toHtml path (Grid databases) =
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


instance toHtmlEntities :: ToHtml (Grid (List Entity)) where
    toHtml path (Grid entities) =
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



grid
    :: forall rowId colId a b
     . ToHtml rowId => ToHtml colId
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
     . ToHtml rowId => ToHtml colId
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
                [ toHtml nodePath colId ]
        wrapRows r =
            [ H.tbody [] r ]
        cells =
                (\(rowId /\ action /\ row) ->
                    rowId /\ action /\ (cellToHtml <$> zip row headers))
            <$> (\(rowId /\ v /\ action) ->
                    rowId /\ action /\ ((/\) rowId <$> getCells rowId v))
            <$> rows
        cellToHtml ((rowId /\ b) /\ colId) = renderCell rowId colId b
        ticker rowId = H.td [ H.classes [ "xodus-ticker" ] ] [ toHtml nodePath rowId ]
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
