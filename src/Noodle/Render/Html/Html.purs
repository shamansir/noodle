module Noodle.Render.Html where

import Prelude

import Data.Int (toNumber)
import Data.Lens (view) as L
import Data.List (toUnfoldable) as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Array as Array
import Data.Sequence as Seq
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)

import Data.Covered (carry, uncover, recover)

--import UI (view, update, update') as UI

import Noodle.API.Errors (NoodleError) as R
import Noodle.Network as R
import Noodle.Optics as L
import Noodle.Path as P
import Noodle.UUID as UUID
import Noodle.Util (Position)
import Noodle.Toolkit (Toolkit, class Channels, ToolkitRenderer) as T

import Noodle.Render.Atom (class Atom, labelOf) as R
import Noodle.Render.Layout as Layout
import Noodle.Render.Layout (PatchLayout, Cell(..), ZIndex(..))
import Noodle.Render.Renderer (Renderer, Routed(..))
import Noodle.Render.Renderer (make) as Renderer
import Noodle.Render.Html.DebugBox (view) as DebugBox

import Noodle.Render.Action (Action(..), RoutedAction, my)
import Noodle.Render.Model
import Noodle.Render.Update (update, updateDebugBox)

import Spork.Html (Html)
import Spork.Html as H
-- import Web.UIEvent.Event as ME
import Web.UIEvent.MouseEvent as ME


type View d c n = Html (RoutedAction d c n)


type HtmlRenderer d c n = Renderer d c n (Action d c n) (Model d c n) (View d c n)


-- type ToolkitRenderer d c = T.ToolkitRenderer d c (View d) Message
type ToolkitRenderer d c n =
    T.ToolkitRenderer d c n
        (View d c n)
        (RoutedAction d c n)
-- FIXME: user might want to use custom messages in the renderer



make
    :: forall d c n
     . T.Channels d c
    => Show d => Show c => Show n
    => R.Atom n
    => ToolkitRenderer d c n
    -> T.Toolkit d c n
    -> HtmlRenderer d c n
make toolkitRenderer toolkit =
    Renderer.make
        toolkit
        (\_ action covered ->
            let
                maybeError /\ (ui /\ nw) = uncover covered
                (ui' /\ effects) = update action (ui /\ nw)
                ui'' = -- FIXME: use <$> over model to update debug box and skip errors
                    ui' { debug = ui'.debug # updateDebugBox nw action }
            in (carry $ ui'' /\ nw) /\ effects)
        (view toolkitRenderer <<< recover)

        -- it is done in Renderer.make: # UI.mapFSM (FSM.joinWith appendErrors


emptyView :: forall d c n. View d c n
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d c n. R.NoodleError -> View d c n
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork
    :: forall d c n
     . R.Atom n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> View d c n
viewNetwork toolkitRenderer ui nw@(R.Network { name, patches }) =
    H.div
        [ H.id_ "network", H.classes [ "noodle-network" ] ]
        $
            [ H.div
                [ H.classes [ "noodle-network-name" ] ]
                [ H.span [] [ H.text name ] ]
            ] <>
            (List.toUnfoldable $ viewPatch toolkitRenderer ui nw
                <$> patches # Seq.toUnfoldable) <>
            [ H.div
                [ H.classes [ "noodle-links" ] ]
                $ (viewLink ui nw <$> allLinks)
                    <> maybe [] (viewDraggingLink ui >>> Array.singleton) currentlyDraggingLink
            ]
    where
        currentlyDraggingLink :: Maybe LinkEnds
        currentlyDraggingLink =
            case ui.dragging of
                NotDragging -> Nothing
                Dragging (DragNode _) -> Nothing
                Dragging (DragLink (R.Outlet outletUuid _ _ _)) ->
                        Map.lookup (UUID.liftTagged outletUuid) ui.positions
                        <#> \outletPos -> { from : outletPos, to : ui.mousePos }
        allLinks :: Array (UUID.ToLink /\ LinkEnds)
        allLinks = Array.catMaybes
            $ Array.concatMap (loadLinks >>> getPositions) (patches # Seq.toUnfoldable)
        loadLinks :: UUID.ToPatch -> Array UUID.ToLink
        loadLinks patchUuid =
            L.view (L._patch patchUuid) nw
                 <#> (\(R.Patch _ _ { links }) -> links # Seq.toUnfoldable)
                  #  fromMaybe []
        getPositions
            :: Array UUID.ToLink
            -> Array (Maybe (UUID.ToLink /\ LinkEnds))
        getPositions links =
            (\linkUuid ->
                case L.view (L._link linkUuid) nw of
                    Just (R.Link _ { inlet, outlet }) ->
                        (\inletPos outletPos -> linkUuid /\ { from : outletPos, to : inletPos })
                            <$> Map.lookup (UUID.liftTagged inlet) ui.positions
                            <*> Map.lookup (UUID.liftTagged outlet) ui.positions
                    Nothing -> Nothing
            ) <$> links


viewPatch
    :: forall d c n
     . R.Atom n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToPatch -- FIXME : make all the views receive the actual instance
    -> View d c n
viewPatch toolkitRenderer ui nw patchUuid =
    case L.view (L._patch patchUuid) nw of
        Just (R.Patch _ patchPath@(P.ToPatch name) { nodes }) ->
            H.div
                [ H.classes [ "noodle-patch" ]
                , uuidToAttr patchUuid
                ]
                [ H.div
                    [ H.classes [ "noodle-patch-name" ] ]
                    [ H.span [] [ H.text name ] ]
                , renderLayout $ Layout.layoutOf patchPath ui.layout
                ]
        _ ->
            H.div
                [ H.classes [ "noodle-missing-patch" ] ]
                [ H.text $ "patch " <> show patchUuid <> " was not found" ]
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
                    $ maybe [] Array.singleton (maybeDragging ui.dragging)
                ]
        renderLayout Nothing =
            H.div [] []
        maybeDragging (Dragging (DragNode (R.Node nodeUuid _ _ _ _ _))) =
            let
                nodePos = toLocalPos ui.positions patchUuid ui.mousePos
            in Just $ viewNode
                toolkitRenderer ui nw (Pinned dragZIndex nodePos) nodeUuid
        maybeDragging _ = Nothing
        showPinnedNode (R.Node nodeUuid _ _ _ _ _) (zIndex /\ pos) =
            viewNode toolkitRenderer ui nw (Pinned zIndex pos) nodeUuid
        showPackedNode (Taken (R.Node nodeUuid _ _ _ _ _)) pos rect =
            viewNode toolkitRenderer ui nw (Packed pos rect) nodeUuid
        showPackedNode Abandoned pos rect =
            H.div [] []


viewNode
    :: forall d c n
     . R.Atom n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> Emplacement
    -> UUID.ToNode -- FIXME : make all the views receive the actual instance
    -> View d c n
viewNode toolkitRenderer ui nw emplacement nodeUuid =
    case L.view (L._node nodeUuid) nw of
        Just node@(R.Node uuid path@(P.ToNode { node : name }) n _ _ { inlets, outlets }) ->
            H.div
                (getAttrs emplacement node)
                (
                    [ H.div
                        [ H.classes [ "noodle-node-title" ]
                        , H.onClick $ handleNodeTitleClick node
                        ]
                        [ H.span [ ] [ H.text $ R.labelOf n <> " (" <> name <> ")" ] ]
                        -- FIXME use special "node title" and "node ID" for classes etc."
                    , H.div
                        [ H.classes [ "noodle-node-remove-button" ]
                        , H.onClick $ handleNodeRemoveButtonClick node
                        ]
                        [ H.text "x" ]
                    , H.div
                        [ H.classes [ "noodle-node-inlets" ] ]
                        $ (viewInlet toolkitRenderer ui nw
                                <$> (inlets # Seq.toUnfoldable))
                    , H.div
                        [ H.classes [ "noodle-node-body" ] ]
                        [ toolkitRenderer.renderNode
                            n
                            node
                            (receiveInletValue path)
                            (receiveOutletValue path)
                        ]
                    , H.div
                        [ H.classes [ "noodle-node-outlets" ] ]
                        $ (viewOutlet toolkitRenderer ui nw
                                <$> (outlets # Seq.toUnfoldable))
                    ]
                )
        _ -> H.div
                [ H.classes [ "noodle-node", "missing" ] ]
                [ H.text $ "node " <> show nodeUuid <> " was not found" ]
    where
        handleNodeTitleClick node e = Just $ my $ ClickNodeTitle node e
        handleNodeRemoveButtonClick node e = Just $ my $ ClickRemoveButton node e
        getAttrs (Pinned (ZIndex zIndex) { x, y }) node =
            let
                (Layout.NodeSize { width, height }) = getNodeSize node
            in
                [ H.classes [ "noodle-node", "noodle-floating" ] -- TODO: toolkit name, node name
                , uuidToAttr nodeUuid
                , H.style $ "transform: translate(" <> show x <> "px, " <> show y <> "px); " <>
                            "min-width: " <> show width <> "px; " <> "height: " <> show height <> "px;" <> " z-index: " <> show zIndex <> "; "
                ]
        getAttrs (Packed { x, y } { width, height }) node =
            [ H.classes [ "noodle-node", "noodle-packed" ] -- TODO: toolkit name, node name
            , uuidToAttr nodeUuid
            , H.style $ "transform: translate(" <> show x <> "px, " <> show y <> "px); " <>
                        "min-width: " <> show width <> "px; " <> "height: " <> show height <> "px;"
            ]
        getAttrs NotDetermined node =
            let
                (Layout.NodeSize { width, height }) = getNodeSize node
            in
                [ H.classes [ "noodle-node" ] -- TODO: toolkit name, node name
                , uuidToAttr nodeUuid
                , H.style $ "min-width: " <> show width <> "px; "
                         <> "height: " <> show height <> "px;"
                ]
        receiveInletValue path inletAlias =
            Map.lookup (P.inletInNode path inletAlias) ui.lastInletData
        receiveOutletValue path outletAlias =
            Map.lookup (P.outletInNode path outletAlias) ui.lastOutletData


viewInlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToInlet -- FIXME : make all the views receive the actual instance
    -> View d c n
viewInlet toolkitRenderer ui nw inletUuid =
    case L.view (L._inlet inletUuid) nw of
        Just inlet@(R.Inlet uuid path@(P.ToInlet { inlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "noodle-inlet" ]
                -- so the whole inlet area is clickable
                , H.onClick $ handleInletConnectorClick inlet
                , uuidToAttr uuid
                ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "noodle-inlet-connector" ]
                    -- , H.onClick $ handleInletConnectorClick inlet
                    ]
                    [ H.text "o" ]
                , H.div [ H.classes [ "noodle-inlet-name" ] ]
                    [ H.span [] [ H.text label ] ]
                , H.div [ H.classes [ "noodle-inlet-value" ] ]
                    [ toolkitRenderer.renderInlet
                        channel
                        inlet
                        $ Map.lookup path ui.lastInletData
                    ]
                ]
        _ -> H.div
                [ H.classes [ "noodle-missing-inlet" ] ]
                [ H.text $ "inlet " <> show inletUuid <> " was not found" ]
    where
        handleInletConnectorClick inletPath e = Just $ my $ ClickInlet inletPath e


viewOutlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToOutlet -- FIXME : make all the views receive the actual instance
    -> View d c n
viewOutlet toolkitRenderer ui nw outletUuid =
    case L.view (L._outlet outletUuid) nw of
        Just outlet@(R.Outlet uuid path@(P.ToOutlet { outlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "noodle-outlet" ]
                -- so the whole inlet area is clickable
                , H.onClick $ handleOutletConnectorClick outlet
                , uuidToAttr uuid
                ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "noodle-outlet-connector" ]
                    --, H.onClick $ handleOutletConnectorClick outlet
                    ]
                    [ H.text "o"
                    ]
                , H.div [ H.classes [ "noodle-outlet-name" ] ]
                    [ H.span [] [ H.text label ] ]
                , H.div [ H.classes [ "noodle-outlet-value" ] ]
                    [ toolkitRenderer.renderOutlet
                        channel
                        outlet
                        $ Map.lookup path ui.lastOutletData
                    ]
                ]
        _ -> H.div
                [ H.classes [ "noodle-missing-outlet" ] ]
                [ H.text $ "outlet " <> show outletUuid <> " was not found" ]
    where
        handleOutletConnectorClick outletPath e = Just $ my $ ClickOutlet outletPath e


viewLink
    :: forall d c n
     . Model d c n
    -> R.Network d c n
    -> UUID.ToLink /\ LinkEnds
    -> View d c n
viewLink _ nw (linkUuid /\ linkPosition) =
    H.div
        [ H.classes [ "noodle-link" ]
        , H.style
            $ getLinkTransformStyle
                $ getLinkTransform linkPosition
        , case L.view (L._link linkUuid) nw of
            Just link -> H.onClick $ handleLinkClick link
            Nothing -> H.onClick $ const Nothing
        ]
        [ ]
    where
        handleLinkClick link e = Just $ my $ ClickLink link e


viewDraggingLink
    :: forall d c n
     . Model d c n
    -> LinkEnds
    -> View d c n
viewDraggingLink _ linkPosition =
    H.div
        [ H.classes [ "noodle-link", "noodle-dragging" ]
        , H.style
            $ getLinkTransformStyle
                $ getLinkTransform linkPosition
        ]
        [ ]


viewDebugWindow
    :: forall d c n
     . Show d => Show c => Show n
    => Model d c n
    -> R.Network d c n
    -> View d c n
viewDebugWindow ui nw =
    H.div
        [ H.id_ "debug" ]
        [ H.input
            [ H.type_ H.InputCheckbox
            , H.checked (isJust ui.debug)
            , H.onChecked
                (H.always_ $ FromUI
                    $ if isJust ui.debug then DisableDebug else EnableDebug)
            ]
        , case ui.debug of
            Just debug -> (FromUI <<< ToDebugBox) <$> DebugBox.view (nw /\ debug)
            _ -> H.div [] []
        ]


-- FIXME: show in DebugBox
viewMousePos :: forall d c n. Position -> View d c n
viewMousePos { x, y } =
    H.span [ H.classes [ "noodle-mouse-pos" ] ] [ H.text $ show x <> ":" <> show y ]


-- FIXME: show in DebugBox
viewDragState :: forall d c n. DragState d c n -> View d c n
viewDragState dragState =
    H.span [ H.classes [ "noodle-drag-state" ] ] [ H.text $ show dragState ]


view
    :: forall d c n
     . Show d => Show c => Show n
    => R.Atom n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n /\ R.Network d c n
    -> View d c n
view toolkitRenderer (ui /\ nw) =
    H.div
        [ H.id_ "html"
        , H.onMouseMove handleMouseMove
        , H.onMouseUp handleMouseUp
        , H.onClick handleClick
        ]
        [ viewDebugWindow ui nw
        -- , case errors of
        --     [] -> H.div [] []
        --     _  -> H.div [] (viewError <$> errors)
        , viewNetwork toolkitRenderer ui nw
        , viewMousePos ui.mousePos
        , viewDragState ui.dragging
        ]
    where
        handleMouseMove e = Just $ my $ MouseMove
            $ { x : toNumber $ ME.clientX e, y : toNumber $ ME.clientY e }
        handleMouseUp e = Just $ my $ MouseUp
            $ { x : toNumber $ ME.clientX e, y : toNumber $ ME.clientY e }
        handleClick e = Just $ my $ ClickBackground e


{-
performEffect
    :: forall d c n
     . T.Toolkit d c n
    -> (Routed (Action d c n) (Core.Action d c n) -> Effect Unit)
    -> Perform d c n
    -> (Model d c n /\ R.Network d c n)
    -> Effect Unit
performEffect _ push UpdatePositions ( ui /\ (R.Network nw) ) = do
    positions <- collectPositions $ loadUUIDs $ Map.keys nw.registry
    push $ my $ StorePositions $ convertPositions positions
performEffect _ push
    (TryConnecting (R.Outlet _ outletPath _ _) (R.Inlet _ inletPath _ _))
    ( ui /\ nw ) = do
    push $ core $ Core.Request $ Core.ToConnect outletPath inletPath
performEffect _ push
    (TryToPinNode node position)
    ( ui /\ nw ) = do
    -- FIXME: it's not an effect, actually
    push $ my $ PinNode node position
performEffect _ push
    (TryRemovingNode node)
    ( ui /\ nw ) = do
    -- FIXME: it's not an effect, actually
    push $ core $ Core.Build $ Core.RemoveNode node
performEffect _ push (StopPropagation e) ( ui /\ nw ) = do
    _ <- Event.stopPropagation e
    pure unit
-}


getLinkTransformStyle :: LinkTransform -> String
getLinkTransformStyle { from, angle, length } =
    "transform: translate(" <> fromPosStr <> ") rotate(" <> angleStr <> ");"
        <> " width: " <> lengthStr <> ";"
    where
        fromPosStr = show from.x <> "px, " <> show from.y <> "px"
        angleStr = show angle <> "rad"
        lengthStr = show length <> "px"



uuidToAttr :: forall a r i. UUID.IsTagged a => a -> H.IProp (id ∷ String | r) i
-- uuidToAttr uuid  = H.prop "uuid" $ UUID.taggedToRawString uuid
uuidToAttr = H.id_ <<< UUID.taggedToRawString


foreign import collectPositions
    :: Array { uuid :: String, kind :: String }
    -> Effect
            (Array
                { uuid :: String
                , kind :: String
                , pos :: Position
                }
            )
