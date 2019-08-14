module Rpd.Renderer.Html where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Lens (view) as L
import Data.Lens.At (at) as L
import Data.List (toUnfoldable, length)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Sequence as Seq
import Data.Tuple.Nested (type (/\), (/\))
import Data.Exists (Exists, mkExists)

import Effect (Effect)

import Rpd.API (RpdError) as R
import Rpd.API.Action (Action(..), DataAction(..), BuildAction(..)) as Core
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.Path as P
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Render.MUV (Renderer(..), PushF(..), skipEffects) as R
import Rpd.Util (type (/->))
import Rpd.Toolkit as T

import Rpd.Renderer.Html.DebugBox as DebugBox

import Spork.Html (Html)
import Spork.Html as H
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
    , dragging :: DragState
    }


type MousePos = Int /\ Int


data Action
    = NoOp
    | StartDrag DragSubject
    | StopDrag
    | MouseMove MousePos
    | EnableDebug
    | DisableDebug
    | StoreLinkPositions (Array Unit)


data Perform
    = UpdateLinksPositions


data DragSubject
    = DragNode UUID.ToNode
    | DragLink UUID.ToOutlet


data DragState
    = NotDragging
    | Dragging DragSubject


type PushF d c n = R.PushF d c n Action


type View d c n = Html (Either Action (Core.Action d c n))


init :: forall d c n. Model d c n
init =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    -- , debug : Nothing
    , debug : Just DebugBox.init
    -- , uuidToChannelDef : Map.empty
    -- , uuidToNodeDef : Map.empty
    , uuidToChannel : Map.empty
    , mousePos : -1 /\ -1
    , dragging : NotDragging
    }


type HtmlRenderer d c n = R.Renderer d c n (Model d c n) (View d c n) Action Perform
-- type ToolkitRenderer d c = T.ToolkitRenderer d c (View d) Message
type ToolkitRenderer d c n = T.ToolkitRenderer d c n (View d c n) (Either Action (Core.Action d c n))
-- FIXME: user might want to use custom messages in the renderer


core :: forall d c n. Core.Action d c n -> Either Action (Core.Action d c n)
core = Right


my :: forall d c n. Action -> Either Action (Core.Action d c n)
my = Left


emptyView :: forall d c n. View d c n
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d c n. R.RpdError -> View d c n
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork
    :: forall d c n
     . T.Channels d c
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
            (toUnfoldable $ viewPatch toolkitRenderer ui nw
                <$> (patches # Seq.toUnfoldable))


viewPatch
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToPatch
    -> View d c n
viewPatch toolkitRenderer ui nw patchUuid =
    case L.view (L._patch patchUuid) nw of
        Just (R.Patch _ (P.ToPatch name) { nodes }) ->
            H.div
                [ H.classes [ "rpd-patch" ] ]
                $ [ H.div
                    [ H.classes [ "rpd-patch-name" ] ]
                    [ H.span [] [ H.text name ] ]
                ] <>
                    (viewNode toolkitRenderer ui nw
                                <$> (nodes # Seq.toUnfoldable))
        _ ->
            H.div
                [ H.classes [ "rpd-missing-patch" ] ]
                [ H.text $ "patch " <> show patchUuid <> " was not found" ]


viewNode
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToNode
    -> View d c n
viewNode toolkitRenderer ui nw nodeUuid =
    case L.view (L._node nodeUuid) nw of
        Just node@(R.Node _ (P.ToNode { node : name }) n _ { inlets, outlets }) ->
            H.div
                [ H.classes [ "rpd-node" ] -- TODO: toolkit name, node name
                , H.style "" ]
                (
                    [ H.div
                        [ H.classes [ "rpd-node-title" ]
                        , H.onClick handleNodeTitleClick
                        ]
                        [ H.span [ ] [ H.text name ] ]
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
                            node ]
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
        handleNodeTitleClick e =
            case ui.dragging of
                NotDragging ->
                    Just $ my $ StartDrag {-(ME.clientX e /\ ME.clientY e)-} $ DragNode nodeUuid
                Dragging _ ->
                    Just $ my $ StopDrag {-(ME.clientX e /\ ME.clientY e)-}


viewInlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToInlet
    -> View d c n
viewInlet toolkitRenderer ui nw inletUuid =
    case L.view (L._inlet inletUuid) nw of
        Just inlet@(R.Inlet _ path@(P.ToInlet { inlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "rpd-inlet" ] ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "rpd-inlet-connector" ]
                    , H.onClick handleInletConnectorClick
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
        handleInletConnectorClick e
            = Nothing


viewOutlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToOutlet
    -> View d c n
viewOutlet toolkitRenderer ui nw outletUuid =
    case L.view (L._outlet outletUuid) nw of
        Just outlet@(R.Outlet _ path@(P.ToOutlet { outlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "rpd-outlet" ] ] -- TODO: channel name, state
                [ H.div
                    [ H.classes [ "rpd-outlet-connector" ]
                    , H.onClick handleOutletConnectorClick
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
        handleOutletConnectorClick e
            = case ui.dragging of
                NotDragging -> Nothing
                Dragging _ -> Just $ my $ StopDrag


viewDebugWindow
    :: forall d c n
     . Show d => Show c => Show n
    => Model d c n
    -> R.Network d c n
    -> View d c n
viewDebugWindow ui nw =
    H.div [ H.id_ "debug" ]
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
    -> Either Action (Core.Action d c n)
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


viewMousePos :: forall d c n. Int /\ Int -> View d c n
viewMousePos ( x /\ y ) =
    H.span [ H.classes [ "rpd-mouse-pos" ] ] [ H.text $ show x <> ":" <> show y ]


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
        , H.onClick handleClick
        ]
        [ viewDebugWindow ui nw
        , viewNetwork toolkitRenderer ui nw
        , viewMousePos ui.mousePos
        ]
    where
        handleMouseMove e = Just $ my $ MouseMove $ ME.clientX e /\ ME.clientY e
        handleClick e =
            case ui.dragging of
                NotDragging -> Nothing
                Dragging _ -> Just $ my $ StopDrag
view _ (Left err) =
    viewError err -- FIXME: show last working network state along with the error


update
    :: forall d c n
     . Either Action (Core.Action d c n)
    -> Model d c n /\ R.Network d c n
    -> Model d c n /\ Array Perform
update (Right (Core.Data Core.Bang)) (ui /\ _) = ui /\ []
update (Right (Core.Data (Core.GotInletData (R.Inlet _ inletPath _ _) d))) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update (Right (Core.Data (Core.GotOutletData (R.Outlet _ outletPath _ _) d))) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update (Right (Core.Build (Core.AddInlet inlet))) ( ui /\ nw ) =
    ( ui /\ [ UpdateLinksPositions ] )
update (Right (Core.Build (Core.AddNode node))) ( ui /\ nw ) =
    ( ui /\ [ UpdateLinksPositions ] )
update (Left (MouseMove mousePos)) ( ui /\ nw ) =
    (ui { mousePos = mousePos })
    /\ []
update _ (ui /\ _) = ui /\ []


performEffect
    :: forall d c n
     . T.Toolkit d c n
    -> (Either Action (Core.Action d c n) -> Effect Unit)
    -> Perform
    -> (Model d c n /\ R.Network d c n)
    -> Effect Unit
performEffect _ pushAction UpdateLinksPositions ( ui /\ nw ) = do
    links <- collectLinksPositions []
    pushAction $ my $ StoreLinkPositions []


foreign import collectLinksPositions
    :: Array String
    -> Effect
            (Array
                { link :: String
                , pos :: { x :: Int, y :: Int }
                }
            )
