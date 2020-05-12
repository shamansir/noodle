module Xodus.Toolkit.Render.Html.Grid where


import Prelude

import Data.Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Spork.Html as H

import Noodle.Path as P

import Xodus.Dto
import Xodus.Toolkit.Value (Value(..))
import Xodus.Toolkit.Render.Html.ToHtml


newtype Grid a = Grid a


instance toHtmlDatbases :: ToHtml (Grid (Array Database)) where
    toHtml path (Grid entityTypes) =
        grid
            []
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


instance toHtmlEntityTypes :: ToHtml (Grid (Array EntityType)) where
    toHtml path (Grid databases) =
        grid'
            []
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


instance toHtmlEntities :: ToHtml (Grid (Array Entity)) where
    toHtml path (Grid entities) =
        grid
            (singleton "xodus-wide-ticker")
            path
            ("type" : (_.name <$> allProperties entities))
            (const valuesOf)
            (\_ _ prop -> H.text prop)
            (mapWithIndex
                (\_ entity@(Entity { id }) ->
                    id
                        /\ entity
                        /\ (Just $ toOutlet path "one" $ SelectOne $ entity)
                ) entities
            )
        where
            valueOrEmpty (Just { value }) = value
            valueOrEmpty Nothing = "-"
            valuesOf entity@(Entity { type : type_ }) =
                type_ :
                    (valueOrEmpty <$> dataOfProperty entity <$> _.name <$> allProperties entities)



instance toHtmlEntity :: ToHtml (Grid Entity) where
    toHtml path (Grid entity) =
        grid
            (singleton "xodus-wide-ticker")
            path
            ("name" : "type" : "value" : [])
            (const valuesOf)
            (\_ _ prop -> H.text prop)
            (mapWithIndex
                (\index prop@{ name } ->
                    index
                        /\ prop
                        /\ Nothing
                ) $ allPropertiesOf entity
            )
        where
            valuesOf { name, type : type_, value } =
                name : type_ : value : []


grid
    :: forall rowId colId a b
     . ToHtml rowId => ToHtml colId
    => Array String
    -> P.ToNode
    -> Array colId
    -> (rowId -> a -> Array b)
    -> (rowId -> colId -> b -> View)
    -> Array (rowId /\ a /\ Maybe Action)
    -> View
grid classes p = grid' classes p Nothing


grid'
    :: forall rowId colId a b
     . ToHtml rowId => ToHtml colId
    => Array String
    -> P.ToNode
    -> Maybe Action
    -> Array colId
    -> (rowId -> a -> Array b)
    -> (rowId -> colId -> b -> View)
    -> Array (rowId /\ a /\ Maybe Action)
    -> View

grid' classes nodePath maybeAll headers getCells renderCell [] =
    H.table
        [ H.classes ([ "xodus-table", "xodus-empty" ] <> toUnfoldable classes) ]
        [ H.tbody
            []
            [
                H.tr
                [ H.classes [ "xodus-first", "xodus-last" ] ]
                [ H.td
                    [ H.classes [ "xodus-first", "xodus-last" ] ]
                    [ H.text "Empty" ]
                ]
            ]
        ]

grid' classes nodePath maybeAll headers getCells renderCell rows =
    H.table
        [ H.classes ("xodus-table" : toUnfoldable classes) ]
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

        ticker rowId = H.td [ H.classes [ "xodus-ticker" ] ] [ toHtml nodePath rowId ]
        emptyTicker = H.th [ H.classes [ "xodus-ticker" ] ] [ H.text "ID" ]

        headerHtml [] = []
        headerHtml headers' =
            [ H.thead [] [ H.tr [] ([ emptyTicker ] <> (headerCell <$> headers')) ] ]
        headerCell (colIdx /\ colId) =
            H.th
                [ H.classes $ ordClasses (length headers) colIdx ]
                [ toHtml nodePath colId ]

        cells =
                (\(rowId /\ action /\ row) ->
                    rowId /\ action /\ (cellToHtml <$> zip row headers))
            <$> (\(rowId /\ v /\ action) ->
                    rowId /\ action /\ ((/\) rowId <$> getCells rowId v))
            <$> rows
        cellToHtml ((rowId /\ b) /\ colId) = renderCell rowId colId b
        dataCell cellCount (cellIdx /\ v) =
            H.td [ H.classes $ ordClasses cellCount cellIdx ] [ v ]

        wrapRows r =
            [ H.tbody [] r ]
        rowHtml (rowIdx /\ (rowId /\ maybeAction /\ row)) =
            H.tr
                (case maybeAction of
                    Just action ->
                        [ H.classes
                            ( "xodus-clickable" : ordClasses (length rows) rowIdx)
                        , H.onClick $ H.always_ $ action
                        ]
                    Nothing ->
                        [ H.classes $ ordClasses (length rows) rowIdx ]
                )
                $ [ ticker rowId ]
                    <> (dataCell (length row) <$> (toUnfoldable $ mapWithIndex ((/\)) row))

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
