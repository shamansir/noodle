module Rpd.Renderer.Html where

import Prelude

import Math (atan2, sqrt, pow)

import Data.Int (toNumber)
import Data.Either (Either(..))
import Data.Either (hush) as Either
import Data.Foldable (fold, foldr)
import Data.Lens (view) as L
import Data.Lens.At (at) as L
import Data.List (List)
import Data.List (List(..), toUnfoldable, length, head) as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Set as Set
import Data.Set (Set)
import Data.Array as Array
import Data.Array ((:))
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Exists (Exists, mkExists)
import Control.Alternative ((<|>))


import Debug.Trace as DT

-- import Debug.Trace as DT

import Effect (Effect)

import Rpd.API (RpdError, uuidByPath) as R
import Rpd.API.Action (Action(..), DataAction(..), BuildAction(..), RequestAction(..)) as Core
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.Path as P
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Render.MUV (Renderer(..), PushF(..), skipEffects) as R
import Rpd.Util (type (/->), (+>), Bounds, Rect, Position)
import Rpd.Toolkit as T

import Rpd.Renderer.Layout as Layout
import Rpd.Renderer.Layout (Layout, PatchLayout, Cell(..), ZIndex(..))
import Rpd.Renderer.Html.DebugBox as DebugBox

import Spork.Html (Html)
import Spork.Html as H
-- import Web.UIEvent.Event as ME
import Web.Event.Event (stopPropagation) as Event
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as ME


type Model d c n =
    -- TODO: use UUID to store inlets?
    { lastInletData :: P.ToInlet /-> d
    , lastOutletData :: P.ToOutlet /-> d
    , debug :: Maybe (DebugBox.Model d c n)
    -- , uuidToChannelDef :: UUID /-> T.ChannelDefAlias
    -- , uuidToNodeDef :: UUID /-> T.NodeDefAlias
    , uuidToChannel :: UUID /-> c
    , mousePos :: MousePos
    , dragging :: DragState d c n
    , positions :: UUID.Tagged /-> Position
    -- , positions ::
    --     { inlets :: UUID.ToInlet /-> Position
    --     , outlets :: UUID.ToOutlet /-> Position
    --     }
    , layout :: Layout d n
    }


-- data ZIndex = ZIndex Int


data Emplacement
    = NotDetermined
    | Pinned ZIndex Position
    | Packed Position Rect


type LinkEnds = { from :: Position, to :: Position }
type LinkTransform = { from :: Position, angle :: Number, length :: Number }


type MousePos = Position


data Action d c n
    = NoOp
    | EnableDebug
    | DisableDebug
    | ClickBackground ME.MouseEvent
    | MouseMove MousePos
    | MouseUp MousePos
    | ClickInlet (R.Inlet d c) ME.MouseEvent
    | ClickOutlet (R.Outlet d c) ME.MouseEvent
    | ClickNodeTitle (R.Node d n) ME.MouseEvent
    | PinNode (R.Node d n) Position
    | StorePositions (UUID.Tagged /-> Position)


data Perform d c n
    = UpdatePositions
    | TryConnecting (R.Outlet d c) (R.Inlet d c)
    | TryToPinNode (R.Node d n) Position
    | StopPropagation Event


data DragSubject d c n
    = DragNode (R.Node d n)
    | DragLink (R.Outlet d c)


data DragState d c n
    = NotDragging
    | Dragging (DragSubject d c n)


instance showDragState :: Show (DragState d c n) where
    show NotDragging = "not dragging"
    show (Dragging (DragNode (R.Node _ nPath _ _ _))) = "dragging node " <> show nPath
    show (Dragging (DragLink (R.Outlet _ oPath _ _))) = "dragging link from " <> show oPath


type PushF d c n = R.PushF d c n (Action d c n)


type View d c n = Html (Either (Action d c n) (Core.Action d c n))


init :: forall d c n. R.Network d c n -> Model d c n
init nw =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    -- , debug : Nothing
    , debug : Just DebugBox.init
    -- , uuidToChannelDef : Map.empty
    -- , uuidToNodeDef : Map.empty
    , uuidToChannel : Map.empty
    , mousePos : { x : -1.0, y : -1.0 }
    , dragging : NotDragging
    , positions : Map.empty -- FIXME: exclude nodes (they are stored in the `packing`)
    , layout :
            Layout.loadIntoStacks defaultLayerSize getNodeSize nw
                # Layout.initWithStacks
    }


type HtmlRenderer d c n = R.Renderer d c n (Model d c n) (View d c n) (Action d c n) (Perform d c n)
-- type ToolkitRenderer d c = T.ToolkitRenderer d c (View d) Message
type ToolkitRenderer d c n =
    T.ToolkitRenderer d c n
        (View d c n)
        (Either (Action d c n) (Core.Action d c n))
-- FIXME: user might want to use custom messages in the renderer


core :: forall d c n. Core.Action d c n -> Either (Action d c n) (Core.Action d c n)
core = Right


my :: forall d c n. Action d c n -> Either (Action d c n) (Core.Action d c n)
my = Left


defaultLayerSize :: Layout.LayerSize
defaultLayerSize = Layout.LayerSize { width : 1000.0, height : 1000.0 }


dragZIndex :: ZIndex
dragZIndex = ZIndex 1000


emptyView :: forall d c n. View d c n
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d c n. R.RpdError -> View d c n
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork
    :: forall d c n
     . Show n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> View d c n
viewNetwork toolkitRenderer ui nw@(R.Network { name, patches }) =
    H.div
        [ H.id_ "network", H.classes [ "rpd-network" ] ]
        $
            [ H.div
                [ H.classes [ "rpd-network-name" ] ]
                [ H.span [] [ H.text name ] ]
            ] <>
            (List.toUnfoldable $ viewPatch toolkitRenderer ui nw
                <$> patches # Seq.toUnfoldable) <>
            [ H.div
                [ H.classes [ "rpd-links" ] ]
                $ (viewLink ui <$> allLinks)
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
     . Show n
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
                [ H.classes [ "rpd-patch" ] ]
                [ H.div
                    [ H.classes [ "rpd-patch-name" ] ]
                    [ H.span [] [ H.text name ] ]
                , renderLayout $ Layout.layoutOf patchPath ui.layout
                ]
        _ ->
            H.div
                [ H.classes [ "rpd-missing-patch" ] ]
                [ H.text $ "patch " <> show patchUuid <> " was not found" ]
    where
        renderLayout :: Maybe (PatchLayout d n) -> View d c n
        renderLayout (Just patchLayout) =
            H.div
                [ H.classes [ "rpd-nodes" ] ]
                [ H.div
                    [ H.classes [ "rpd-packed-nodes" ] ]
                    $ Layout.withStackOf showPackedNode patchLayout
                , H.div
                    [ H.classes [ "rpd-pinned-nodes" ] ]
                    $ Layout.withPinnedOf showPinnedNode patchLayout
                , H.div
                    [ H.classes [ "rpd-dragged-nodes" ] ]
                    $ maybe [] Array.singleton (maybeDragging ui.dragging)
                ]
        renderLayout Nothing =
            H.div [] []
        maybeDragging (Dragging (DragNode (R.Node nodeUuid _ _ _ _))) =
            Just $ viewNode toolkitRenderer ui nw (Pinned dragZIndex ui.mousePos) nodeUuid
        maybeDragging _ = Nothing
        showPinnedNode (R.Node nodeUuid _ _ _ _) (zIndex /\ pos) =
            viewNode toolkitRenderer ui nw (Pinned zIndex pos) nodeUuid
        showPackedNode (Taken (R.Node nodeUuid _ _ _ _)) pos rect =
            viewNode toolkitRenderer ui nw (Packed pos rect) nodeUuid
        showPackedNode Abandoned pos rect =
            H.div [] []


viewNode
    :: forall d c n
     . Show n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> Emplacement
    -> UUID.ToNode -- FIXME : make all the views receive the actual instance
    -> View d c n
viewNode toolkitRenderer ui nw emplacement nodeUuid =
    case L.view (L._node nodeUuid) nw of
        Just node@(R.Node uuid path@(P.ToNode { node : name }) n _ { inlets, outlets }) ->
            H.div
                (getAttrs emplacement node)
                (
                    [ H.div
                        [ H.classes [ "rpd-node-title" ]
                        , H.onClick $ handleNodeTitleClick node
                        ]
                        [ H.span [ ] [ H.text $ name <> " (" <> show n <> ")" ] ]
                    , H.div
                        [ H.classes [ "rpd-node-remove-button" ] ]
                        [ H.text "x" ]
                    , H.div
                        [ H.classes [ "rpd-node-inlets" ] ]
                        $ (viewInlet toolkitRenderer ui nw
                                <$> (inlets # Seq.toUnfoldable))
                    , H.div
                        [ H.classes [ "rpd-node-body" ] ]
                        [ toolkitRenderer.renderNode
                            n
                            node
                            (receiveInletValue path)
                            (receiveOutletValue path)
                        ]
                    , H.div
                        [ H.classes [ "rpd-node-outlets" ] ]
                        $ (viewOutlet toolkitRenderer ui nw
                                <$> (outlets # Seq.toUnfoldable))
                    ]
                )
        _ -> H.div
                [ H.classes [ "rpd-node", "missing" ] ]
                [ H.text $ "node " <> show nodeUuid <> " was not found" ]
    where
        handleNodeTitleClick nodePath e = Just $ my $ ClickNodeTitle nodePath e
        getAttrs (Pinned (ZIndex zIndex) { x, y }) node =
            let
                (Layout.NodeSize { width, height }) = getNodeSize node
            in
                [ H.classes [ "rpd-node", "rpd-floating" ] -- TODO: toolkit name, node name
                , uuidToAttr nodeUuid
                , H.style $ "transform: translate(" <> show x <> "px, " <> show y <> "px); " <>
                            "min-width: " <> show width <> "px; " <> "min-height: " <> show height <> "px;" <> " z-index: " <> show zIndex <> "; "
                ]
        getAttrs (Packed { x, y } { width, height }) node =
            [ H.classes [ "rpd-node", "rpd-packed" ] -- TODO: toolkit name, node name
            , uuidToAttr nodeUuid
            , H.style $ "transform: translate(" <> show x <> "px, " <> show y <> "px); " <>
                        "min-width: " <> show width <> "px; " <> "min-height: " <> show height <> "px;"
            ]
        getAttrs NotDetermined node =
            let
                (Layout.NodeSize { width, height }) = getNodeSize node
            in
                [ H.classes [ "rpd-node" ] -- TODO: toolkit name, node name
                , uuidToAttr nodeUuid
                , H.style $ "min-width: " <> show width <> "px; "
                        <> "min-height: " <> show height <> "px;"
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
                [ H.classes [ "rpd-inlet" ]
                , uuidToAttr uuid
                ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "rpd-inlet-connector" ]
                    , H.onClick $ handleInletConnectorClick inlet
                    ]
                    [ H.text "o" ]
                , H.div [ H.classes [ "rpd-inlet-name" ] ]
                    [ H.span [] [ H.text label ] ]
                , H.div [ H.classes [ "rpd-inlet-value" ] ]
                    [ toolkitRenderer.renderInlet
                        channel
                        inlet
                        $ Map.lookup path ui.lastInletData
                    ]
                ]
        _ -> H.div
                [ H.classes [ "rpd-missing-inlet" ] ]
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
                [ H.classes [ "rpd-outlet" ]
                , uuidToAttr uuid
                ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "rpd-outlet-connector" ]
                    , H.onClick $ handleOutletConnectorClick outlet
                    ]
                    [ H.text "o"
                    ]
                , H.div [ H.classes [ "rpd-outlet-name" ] ]
                    [ H.span [] [ H.text label ] ]
                , H.div [ H.classes [ "rpd-outlet-value" ] ]
                    [ toolkitRenderer.renderOutlet
                        channel
                        outlet
                        $ Map.lookup path ui.lastOutletData
                    ]
                ]
        _ -> H.div
                [ H.classes [ "rpd-missing-outlet" ] ]
                [ H.text $ "outlet " <> show outletUuid <> " was not found" ]
    where
        handleOutletConnectorClick outletPath e = Just $ my $ ClickOutlet outletPath e


viewLink
    :: forall d c n
     . Model d c n
    -> UUID.ToLink /\ LinkEnds
    -> View d c n
viewLink _ (uuid /\ linkPosition) =
    H.div
        [ H.classes [ "rpd-link" ]
        , H.style
            $ getLinkTransformStyle
                $ getLinkTransform linkPosition
        ]
        [ H.text "LINK" ]


viewDraggingLink
    :: forall d c n
     . Model d c n
    -> LinkEnds
    -> View d c n
viewDraggingLink _ linkPosition =
    H.div
        [ H.classes [ "rpd-link", "rpd-dragging" ]
        , H.style
            $ getLinkTransformStyle
                $ getLinkTransform linkPosition
        ]
        [ H.text "LINK" ]


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
                (H.always_ $ Left
                    $ if isJust ui.debug then DisableDebug else EnableDebug)
            ]
        , case ui.debug of
            Just debug -> (const $ Left NoOp) <$> DebugBox.view nw debug
            _ -> H.div [] []
        ]


updateDebugBox
    :: forall d c n
     . R.Network d c n
    -> Either (Action d c n) (Core.Action d c n)
    -> Maybe (DebugBox.Model d c n)
    -> Maybe (DebugBox.Model d c n)
updateDebugBox nw (Right action) (Just debug) = Just $ DebugBox.update action nw debug
updateDebugBox _ (Left EnableDebug) _ = Just $ DebugBox.init
updateDebugBox _ (Left DisableDebug) _ = Nothing
updateDebugBox _ _ v = v


htmlRenderer
    :: forall d c n
     . T.Channels d c
    => Show d => Show c => Show n
    => ToolkitRenderer d c n
    -> HtmlRenderer d c n
htmlRenderer toolkitRenderer =
    R.Renderer
        { from : emptyView
        , init : init
        , update :
            \toolkit action (ui /\ nw) ->
                let
                    (ui' /\ effects) = update action (ui /\ nw)
                    ui'' =
                        ui' { debug = ui'.debug # updateDebugBox nw action }
                in (ui'' /\ effects)
        , view : view toolkitRenderer
        , performEffect
        }


-- FIXME: show in DebugBox
viewMousePos :: forall d c n. Position -> View d c n
viewMousePos { x, y } =
    H.span [ H.classes [ "rpd-mouse-pos" ] ] [ H.text $ show x <> ":" <> show y ]


-- FIXME: show in DebugBox
viewDragState :: forall d c n. DragState d c n -> View d c n
viewDragState dragState =
    H.span [ H.classes [ "rpd-drag-state" ] ] [ H.text $ show dragState ]


view
    :: forall d c n
     . Show d => Show c => Show n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> Either R.RpdError (Model d c n /\ R.Network d c n)
    -> View d c n
view toolkitRenderer (Right (ui /\ nw)) =
    H.div
        [ H.id_ "html"
        , H.onMouseMove handleMouseMove
        , H.onMouseUp handleMouseUp
        , H.onClick handleClick
        ]
        [ viewDebugWindow ui nw
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
view _ (Left err) =
    viewError err -- FIXME: show last working network state along with the error


update
    :: forall d c n
     . Either (Action d c n) (Core.Action d c n)
    -> Model d c n /\ R.Network d c n
    -> Model d c n /\ Array (Perform d c n)

update (Right (Core.Data Core.Bang)) (ui /\ _) = ui /\ []
update (Right (Core.Data (Core.GotInletData (R.Inlet _ inletPath _ _) d))) (ui /\ _) =
    ui { lastInletData = ui.lastInletData # Map.insert inletPath d }
    /\ []
update (Right (Core.Data (Core.GotOutletData (R.Outlet _ outletPath _ _) d))) (ui /\ _) =
    ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d }
    /\ []
update (Right (Core.Build (Core.AddInlet _))) ( ui /\ nw ) =
    ui /\ [ UpdatePositions ]
update (Right (Core.Build (Core.AddNode node@(R.Node nodeUuid nodePath _ _ _)))) ( ui /\ nw ) =
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
            /\ [ UpdatePositions ]
        _ ->
            ui /\ []
    where
        patchPath = P.getPatchPath $ P.lift nodePath
update (Right (Core.Build (Core.AddLink _))) ( ui /\ nw ) =
    ui /\ [ UpdatePositions ]
update (Right _) (ui /\ _) = ui /\ []

update (Left NoOp) (ui /\ _) = ui /\ []
update (Left (MouseMove mousePos)) ( ui /\ nw ) =
    ui { mousePos = mousePos } /\
        (case ui.dragging of
            Dragging (DragNode _) -> [ UpdatePositions ]
            _ -> []
        )
update (Left (MouseUp mousePos)) ( ui /\ nw ) =
    ui { mousePos = mousePos }
    /\ pinNodeIfDragging ui.dragging
    where
        pinNodeIfDragging (Dragging (DragNode node)) =
            [ TryToPinNode node mousePos ]
        pinNodeIfDragging _ = [] -- discard link if dragging it
update (Left (PinNode node position)) ( ui /\ nw ) =
    let
        (R.Node _ nodePath _ _ _) = node
        patchPath = P.getPatchPath $ P.lift nodePath
        maybePatch = L.view (L._patchByPath patchPath) nw
    in
        case maybePatch of
            Just patch ->
                ui
                    { layout =
                        ui.layout # Layout.pinAt patch node position
                    }
            Nothing -> ui
        /\ []
update (Left (ClickBackground e)) ( ui /\ nw ) =
    ui { dragging = NotDragging } /\ []
update (Left (ClickNodeTitle node e)) ( ui /\ nw ) =
    let
        (R.Node _ nodePath _ _ _) = node
        patchPath = P.getPatchPath $ P.lift nodePath
        maybePatch = L.view (L._patchByPath patchPath) nw
    in
        case maybePatch of
            Just patch ->
                ui
                    { dragging = Dragging $ DragNode node
                    , layout =
                        ui.layout # Layout.abandon patch node
                    }
            Nothing ->
                ui
                    { dragging = Dragging $ DragNode node }
        /\
            [ StopPropagation $ ME.toEvent e
            , UpdatePositions
            ]
update (Left (ClickInlet inletPath e)) ( ui /\ nw ) =
    ui
        { dragging = NotDragging
        -- TODO: if node was dragged before, place it at the mouse point
        }
    /\ (case ui.dragging of
        Dragging (DragLink outletPath) ->
            [ StopPropagation $ ME.toEvent e
            , TryConnecting outletPath inletPath
            ]
        _ -> [ StopPropagation $ ME.toEvent e ])
update (Left (ClickOutlet outletPath e)) ( ui /\ nw ) =
    ui
        { dragging = Dragging $ DragLink outletPath
        -- TODO: if node was dragged before, place it at the mouse point
        }
    /\ [ StopPropagation $ ME.toEvent e ]
update (Left EnableDebug) (ui /\ _) = ui /\ [] -- TODO: why do nothing?
update (Left DisableDebug) (ui /\ _) = ui /\ [] -- TODO: why do nothing?
update (Left (StorePositions positions)) (ui /\ _) =
    ui { positions = positions } /\ []


performEffect
    :: forall d c n
     . T.Toolkit d c n
    -> (Either (Action d c n) (Core.Action d c n) -> Effect Unit)
    -> Perform d c n
    -> (Model d c n /\ R.Network d c n)
    -> Effect Unit
performEffect _ pushAction UpdatePositions ( ui /\ (R.Network nw) ) = do
    positions <- collectPositions $ loadUUIDs $ Map.keys nw.registry
    -- let _ = DT.spy "positions" $ convertPositions positions
    pushAction $ my $ StorePositions $ convertPositions positions
performEffect _ pushAction
    (TryConnecting (R.Outlet _ outletPath _ _) (R.Inlet _ inletPath _ _))
    ( ui /\ nw ) = do
    pushAction $ core $ Core.Request $ Core.ToConnect outletPath inletPath
performEffect _ pushAction
    (TryToPinNode node position)
    ( ui /\ nw ) = do
    -- FIXME: it's not an effect, actually
    pushAction $ my $ PinNode node position
performEffect _ pushAction (StopPropagation e) ( ui /\ nw ) = do
    _ <- Event.stopPropagation e
    pure unit



getNodeSize :: forall d n. R.Node d n -> Layout.NodeSize
getNodeSize _ = Layout.NodeSize { width : 100.0, height : 100.0 }


uuidToAttr :: forall a r i. UUID.IsTagged a => a -> H.IProp (id ∷ String | r) i
-- uuidToAttr uuid  = H.prop "uuid" $ UUID.taggedToRawString uuid
uuidToAttr = H.id_ <<< UUID.taggedToRawString


loadUUIDs :: Set UUID.Tagged -> Array { uuid :: String, kind :: String }
loadUUIDs uuids = UUID.encode <$> Set.toUnfoldable uuids


-- TODO: move to Renderer.Layout?
convertPositions
    :: Array
            { uuid :: String
            , kind :: String
            , pos :: Position
            }
    -> UUID.Tagged /-> Position
convertPositions srcPositions =
    Map.fromFoldable $ Array.catMaybes $ decodePos <$> srcPositions
    where
        decodePos
            ::
                { uuid :: String
                , kind :: String
                , pos :: Position
                }
            -> Maybe (UUID.Tagged /\ Position)
        decodePos { kind, uuid, pos } =
            (\tagged -> tagged /\ pos) <$> UUID.decode { uuid, kind }


getLinkTransform :: { from :: Position, to :: Position } -> LinkTransform
getLinkTransform { from, to } =
    let
        vector = { x: to.x - from.x, y : to.y - from.y }
        angle = atan2 vector.y vector.x
        length = sqrt $ pow vector.x 2.0 + pow vector.y 2.0
    in
        { from, angle, length }


getLinkTransformStyle :: LinkTransform -> String
getLinkTransformStyle { from, angle, length } =
    "transform: translate(" <> fromPosStr <> ") rotate(" <> angleStr <> ");"
        <> " width: " <> lengthStr <> ";"
    where
        fromPosStr = show from.x <> "px, " <> show from.y <> "px"
        angleStr = show angle <> "rad"
        lengthStr = show length <> "px"


foreign import collectPositions
    :: Array { uuid :: String, kind :: String }
    -> Effect
            (Array
                { uuid :: String
                , kind :: String
                , pos :: Position
                }
            )
