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


type Model d c n =
    -- TODO: use UUID to store inlets?
    { lastInletData :: P.ToInlet /-> d
    , lastOutletData :: P.ToOutlet /-> d
    , debug :: Maybe (DebugBox.Model d c n)
    -- , uuidToChannelDef :: UUID /-> T.ChannelDefAlias
    -- , uuidToNodeDef :: UUID /-> T.NodeDefAlias
    , uuidToChannel :: UUID /-> c
    }


data Action
    = NoOp
    | ClickAt (Int /\ Int)
    | EnableDebug
    | DisableDebug


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
    }


type HtmlRenderer d c n = R.Renderer d c n (Model d c n) (View d c n) Action Unit
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
    -> PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> View d c n
viewNetwork toolkitRenderer pushMsg ui nw@(R.Network { name, patches }) =
    H.div
        [ H.id_ "network", H.classes [ "rpd-network" ] ]
        $
            [ H.div
                [ H.classes [ "rpd-network-name" ] ]
                [ H.text name ]
            ] <>
            (toUnfoldable $ viewPatch toolkitRenderer pushMsg ui nw
                <$> (patches # Seq.toUnfoldable))


viewPatch
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToPatch
    -> View d c n
viewPatch toolkitRenderer pushMsg ui nw patchUuid =
    case L.view (L._patch patchUuid) nw of
        Just (R.Patch _ (P.ToPatch name) { nodes }) ->
            H.div
                [ H.classes [ "rpd-patch" ] ]
                $ [ H.div
                    [ H.classes [ "rpd-patch-name" ] ]
                    [ H.text name ]
                ] <>
                    (viewNode toolkitRenderer pushMsg ui nw
                                <$> (nodes # Seq.toUnfoldable))
        _ ->
            H.div
                [ H.classes [ "rpd-missing-patch" ] ]
                [ H.text $ "patch " <> show patchUuid <> " was not found" ]

viewNode
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToNode
    -> View d c n
viewNode toolkitRenderer pushMsg ui nw nodeUuid =
    case L.view (L._node nodeUuid) nw of
        Just node@(R.Node _ (P.ToNode { node : name }) n _ { inlets, outlets }) ->
            H.div
                [ H.classes [ "rpd-node" ] -- TODO: toolkit name, node name
                , H.style "" ]
                (
                    [ H.div [ H.classes [ "rpd-title" ] ] [ H.text name ]
                    , H.div
                        [ H.classes [ "rpd-inlets" ] ]
                        $ (viewInlet toolkitRenderer pushMsg ui nw
                                <$> (inlets # Seq.toUnfoldable))
                    , H.div
                        [ H.classes [ "rpd-remove-button" ] ]
                        [ H.text "x" ]
                    , H.div
                        [ H.classes [ "rpd-body" ] ]
                        [ toolkitRenderer.renderNode
                            n
                            node
                            (case pushMsg of R.PushF f -> f) ]
                    , H.div
                        [ H.classes [ "rpd-outlets" ] ]
                        $ (viewOutlet toolkitRenderer pushMsg ui nw
                                <$> (outlets # Seq.toUnfoldable))
                    ]
                )
        _ -> H.div
                [ H.classes [ "rpd-missing-node" ] ]
                [ H.text $ "node " <> show nodeUuid <> " was not found" ]


viewInlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToInlet
    -> View d c n
viewInlet toolkitRenderer pushMsg ui nw inletUuid =
    case L.view (L._inlet inletUuid) nw of
        Just inlet@(R.Inlet _ path@(P.ToInlet { inlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "rpd-inlet" ] ] -- TODO: channel name, state
                [ H.div [ H.classes [ "rpd-connector" ] ] [ H.text "o" ]
                , H.div [ H.classes [ "rpd-name" ] ] [ H.text label ]
                , H.div [ H.classes [ "rpd-value" ] ]
                    [ case Map.lookup path ui.lastInletData of
                        Just d -> H.text "<data>"
                        _ -> H.text "<x>"
                    ]
                , H.div [] [ toolkitResult inlet channel ]
                ]
        _ -> H.div
                [ H.classes [ "rpd-missing-inlet" ] ]
                [ H.text $ "inlet " <> show inletUuid <> " was not found" ]
    where
        toolkitResult inlet channel =
            toolkitRenderer.renderInlet
                channel
                inlet
                (case pushMsg of R.PushF f -> f)


viewOutlet
    :: forall d c n
     . T.Channels d c
    => ToolkitRenderer d c n
    -> PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> UUID.ToOutlet
    -> View d c n
viewOutlet toolkitRenderer pushMsg ui nw outletUuid =
    case L.view (L._outlet outletUuid) nw of
        Just outlet@(R.Outlet _ path@(P.ToOutlet { outlet : label }) channel { flow }) ->
            H.div
                [ H.classes [ "rpd-outlet" ] ] -- TODO: channel name, state
                [ H.div [ H.classes [ "rpd-connector" ] ] [ H.text "o" ]
                , H.div [ H.classes [ "rpd-name" ] ] [ H.text label ]
                , H.div [ H.classes [ "rpd-value" ] ]
                    [ case Map.lookup path ui.lastOutletData of
                        Just d -> H.text "<data>"
                        _ -> H.text "<x>"
                    ]
                , H.div [] [ toolkitResult outlet channel ]
                ]
        _ -> H.div
                [ H.classes [ "outlet" ] ]
                [ H.text $ "outlet " <> show outletUuid <> " was not found" ]
    where
        toolkitResult outlet channel =
            toolkitRenderer.renderOutlet
                channel
                outlet
                (case pushMsg of R.PushF f -> f)


viewDebugWindow
    :: forall d c n
     . Show d => Show c => Show n
    => PushF d c n
    -> Model d c n
    -> R.Network d c n
    -> View d c n
viewDebugWindow pushMsg ui nw =
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
        , performEffect : R.skipEffects
        }


view
    :: forall d c n
     . Show d => Show c => Show n
    => T.Channels d c
    => ToolkitRenderer d c n
    -> PushF d c n
    -> Either R.RpdError (Model d c n /\ R.Network d c n)
    -> View d c n
view toolkitRenderer pushMsg (Right (ui /\ nw)) =
    H.div [ H.id_ "html" ]
        [ viewDebugWindow pushMsg ui nw
        , viewNetwork toolkitRenderer pushMsg ui nw
        ]
view _ pushMsg (Left err) =
    viewError err -- FIXME: show last working network state along with the error


update
    :: forall d c n
     . Either Action (Core.Action d c n)
    -> Model d c n /\ R.Network d c n
    -> Model d c n /\ Array Unit
update (Right (Core.Data Core.Bang)) (ui /\ _) = ui /\ []
update (Right (Core.Data (Core.GotInletData (R.Inlet _ inletPath _ _) d))) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update (Right (Core.Data (Core.GotOutletData (R.Outlet _ outletPath _ _) d))) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update (Right (Core.Build (Core.AddInlet inlet))) ( ui /\ nw ) =
    ( ui /\ [] )
update _ (ui /\ _) = ui /\ []

