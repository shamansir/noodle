module Noodle.Render.Update where

import Prelude

import Effect (Effect)

import FSM (doNothing, single, batch, just)

import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Lens (view) as L

import Noodle.Network as R
import Noodle.API.Action (Action(..), DataAction(..), BuildAction(..), RequestAction(..)) as Core
import Noodle.Path as P
import Noodle.Optics as L

import Noodle.Render.Model
import Noodle.Render.Action
import Noodle.Render.Renderer (Routed(..))
import Noodle.Render.DebugBox (Model, init, update) as DebugBox
import Noodle.Render.Layout as Layout

import Web.Event.Event (stopPropagation) as Event
-- import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as ME


update
    :: forall d c n
     . RoutedAction d c n
    -> Model d c n /\ R.Network d c n
    -> Model d c n /\ List (Effect (RoutedAction d c n))

update (FromCore (Core.Data (Core.GotInletData (R.Inlet _ inletPath _ _) d))) (ui /\ nw) =
    ui { lastInletData = ui.lastInletData # Map.insert inletPath d }
    /\ (just $ updatePositions (my <<< StorePositions) nw)
update (FromCore (Core.Data (Core.GotOutletData (R.Outlet _ outletPath _ _) d))) (ui /\ nw) =
    ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d }
    /\ (just $ updatePositions (my <<< StorePositions) nw)
update (FromCore (Core.Build (Core.AddNode node@(R.Node nodeUuid nodePath _ _ _ _)))) ( ui /\ nw ) =
    case L.view (L._patchByPath patchPath) nw of
        -- FIXME: Raise the error if patch wasn't found
        Just patch ->
            ui
                { layout =
                    Layout.pack
                        defaultLayerSize
                        (getNodeSize node)
                        patch
                        node
                        ui.layout
                }
                -- TODO: remove from packing when node was removed
            /\ (just $ updatePositions (my <<< StorePositions) nw)
        _ ->
            ui /\ doNothing
    where
        patchPath = P.getPatchPath $ P.lift nodePath
update (FromCore _) (ui /\ nw) =
    ui /\ (just $ updatePositions (my <<< StorePositions) nw)

update (FromUI NoOp) (ui /\ _) = ui /\ doNothing
--update (FromUI (Batch actions)) (ui /\ _) = ui /\ doNothing -- FIXME: implement
update (FromUI (MouseMove mousePos)) ( ui /\ nw ) =
    ui { mousePos = mousePos } /\
        (case ui.dragging of
            Dragging (DragNode _) ->
                just $ updatePositions (my <<< StorePositions) nw
            _ -> doNothing
        )
update (FromUI (MouseUp mousePos)) ( ui /\ nw ) =
    ui { mousePos = mousePos }
    /\ pinNodeIfDragging ui.dragging
    where
        pinNodeIfDragging (Dragging (DragNode node)) =
            single $ my $ PinNode node mousePos
        pinNodeIfDragging _ = doNothing -- discard link if dragging it
update (FromUI (PinNode node position)) ( ui /\ nw ) =
    let
        (R.Node _ nodePath _ _ _ _) = node
        patchPath = P.getPatchPath $ P.lift nodePath
        maybePatch = L.view (L._patchByPath patchPath) nw
    in
        case maybePatch of
            Just patch@(R.Patch patchUuid _ _) ->
                let localPosition = toLocalPos ui.positions patchUuid position
                in ui
                    { layout =
                        ui.layout # Layout.pinAt patch node localPosition
                    }
            Nothing -> ui
        /\ doNothing
update (FromUI (ClickBackground e)) ( ui /\ nw ) =
    ui { dragging = NotDragging } /\ doNothing
update (FromUI (ClickNodeTitle node e)) ( ui /\ nw ) =
    ui
        { dragging = Dragging $ DragNode node
        , layout =
            let
                (R.Node _ nodePath _ _ _ _) = node
                patchPath = P.getPatchPath $ P.lift nodePath
                maybePatch = L.view (L._patchByPath patchPath) nw
            in
                case maybePatch of
                    Just patch ->
                        ui.layout # Layout.abandon patch node
                    Nothing -> ui.layout
        }
    /\ (
        (do
            _ <- Event.stopPropagation $ ME.toEvent e
            pure $ my NoOp)
        : updatePositions (my <<< StorePositions) nw
        : Nil
    )
update (FromUI (ClickRemoveButton node@(R.Node _ nodePath _ _ _ _) e)) ( ui /\ nw ) =
    ui
        { dragging = NotDragging
        , layout =
            let
                patchPath = P.getPatchPath $ P.lift nodePath
                maybePatch = L.view (L._patchByPath patchPath) nw
            in
                case maybePatch of
                    Just patch ->
                        ui.layout # Layout.remove patch node
                    Nothing -> ui.layout
        }
    /\ (
        (do
            _ <- Event.stopPropagation $ ME.toEvent e
            pure $ my NoOp)
        : (pure $ core $ Core.Request $ Core.ToRemoveNode nodePath)
        : Nil
    )
update (FromUI (ClickInlet (R.Inlet _ inletPath _ _) e)) ( ui /\ nw ) =
    ui
        { dragging = NotDragging
        -- TODO: if node was dragged before, place it at the mouse point
        }
    /\ (
        (do
            _ <- Event.stopPropagation $ ME.toEvent e
            pure $ my NoOp)
        : (case ui.dragging of
            Dragging (DragLink (R.Outlet _ outletPath _ _)) ->
                pure $ core $ Core.Request $ Core.ToConnect outletPath inletPath
            _ -> pure $ my NoOp)
        : Nil
    )
update (FromUI (ClickOutlet outletPath e)) ( ui /\ nw ) =
    ui
        { dragging = Dragging $ DragLink outletPath
        -- TODO: if node was dragged before, place it at the mouse point
        }
    /\ (just $ do
        _ <- Event.stopPropagation $ ME.toEvent e
        pure $ my NoOp)
update (FromUI (ClickLink link e)) ( ui /\ nw ) =
    ui
    /\ (
        (do
            _ <- Event.stopPropagation $ ME.toEvent e
            pure $ my NoOp)
        -- it seems right, not to request to disconnect, rather to remove this particular link
        : (pure $ core $ Core.Build $ Core.RemoveLink link)
        : (updatePositions (my <<< StorePositions) nw)
        : Nil)
update (FromUI EnableDebug) (ui /\ _) = ui /\ doNothing -- TODO: why do nothing?
update (FromUI DisableDebug) (ui /\ _) = ui /\ doNothing -- TODO: why do nothing?
update (FromUI (StorePositions positions)) (ui /\ _) =
    ui { positions = positions } /\ doNothing
update (FromUI (ToDebugBox debugBoxAction)) (ui /\ nw) =
    ui { debug =
        (\debug -> DebugBox.update (Left debugBoxAction) (nw /\ debug))
            <$> ui.debug
        }
    /\ doNothing


updateDebugBox
    :: forall d c n
     . R.Network d c n
    -> RoutedAction d c n
    -> Maybe (DebugBox.Model d c n)
    -> Maybe (DebugBox.Model d c n)
updateDebugBox nw (FromCore action) (Just debug) =
    Just $ DebugBox.update (Right action) (nw /\ debug)
updateDebugBox _ (FromUI EnableDebug) _ = Just $ DebugBox.init
updateDebugBox _ (FromUI DisableDebug) _ = Nothing
updateDebugBox _ _ v = v
