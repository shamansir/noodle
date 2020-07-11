module Noodle.Render.Component.Patch.Html where

import Prelude (($), (<$>))

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Array (singleton) as Array

import Noodle.Network (Patch(..), Node(..)) as R
import Noodle.Render.Action (core) as R
import Noodle.Render.Html (View)
import Noodle.Render.Layout as Layout
import Noodle.Render.Layout (PatchLayout, Cell(..), ZIndex(..))
import Noodle.Render.Atom (class Atom, labelOf)
import Noodle.API.Action (Action(..), RequestAction(..)) as A
import Noodle.Path (ToPatch(..)) as P
import Noodle.Util (Position)

import Spork.Html as H


import Noodle.Render.Component.Patch.Model


render :: forall d c n. Atom n => Position -> Model d c n -> View d c n
render mousePos { dragging, positions, patch } = render' mousePos dragging positions patch


render' :: forall d c n. Atom n => DragSubject d c n -> Positions -> R.Patch d c n -> View d c n
render' mousePos dragging positions (R.Patch patchUuid patchPath@(P.ToPatch name) { nodes }) =
    H.div
        [ H.classes [ "noodle-patch" ]
        , uuidToAttr patchUuid
        ]
        [ H.div
            [ H.classes [ "noodle-patch-name" ] ]
            [ H.span [] [ H.text name ] ]
        , renderLayout $ Layout.layoutOf patchPath ui.layout
        ]
    where
        renderLayout :: Maybe (PatchLayout d n) -> View d c n
        renderLayout (Just patchLayout) =
            H.div
                [ H.classes [ "noodle-nodes" ] ]
                [ H.div
                    [ H.classes [ "noodle-packed-nodes" ] ]
                    $ Layout.withStackOf showPackedNode patchLayout
                , H.div
                    [ H.classes [ "noodle-pinned-nodes" ] ]
                    $ Layout.withPinnedOf showPinnedNode patchLayout
                , H.div
                    [ H.classes [ "noodle-dragged-nodes" ] ]
                    $ maybe [] Array.singleton (maybeDragging dragging)
                ]
        renderLayout Nothing =
            H.div [] []
        maybeDragging (Dragging (DragNode (R.Node nodeUuid _ _ _ _ _))) =
            let
                nodePos = toLocalPos positions patchUuid mousePos
            in Just $ viewNode
                toolkitRenderer ui nw (Pinned dragZIndex nodePos) nodeUuid
        maybeDragging _ = Nothing
        showPinnedNode (R.Node nodeUuid _ _ _ _ _) (zIndex /\ pos) =
            viewNode toolkitRenderer ui nw (Pinned zIndex pos) nodeUuid
        showPackedNode (Taken (R.Node nodeUuid _ _ _ _ _)) pos rect =
            viewNode toolkitRenderer ui nw (Packed pos rect) nodeUuid
        showPackedNode Abandoned pos rect =
            H.div [] []
