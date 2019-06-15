module Rpd.Renderer.Html where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Lens (view) as L
import Data.Lens.At (at) as L
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Data.Sequence as Seq
import Data.Tuple.Nested (type (/\), (/\))
import Data.Exists (Exists, mkExists)
import Debug.Trace (spy)

import Rpd.API (RpdError) as R
import Rpd.Command (Command(..)) as C
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.Path as P
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Render as R
import Rpd.Render.MUV (Renderer(..), PushF(..)) as R
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


data Message
    = NoOp
    | ClickAt (Int /\ Int)
    | EnableDebug
    | DisableDebug


type PushF d c n = R.PushF Message (C.Command d c n)


type View d c n = Html (Either Message (C.Command d c n))


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


type HtmlRenderer d c n = R.Renderer d c n (Model d c n) (View d c n) Message
-- type ToolkitRenderer d c = T.ToolkitRenderer d c (View d) Message
type ToolkitRenderer d c n = T.ToolkitRenderer d c n (View d c n) (Either Message (C.Command d c n))
-- FIXME: user could want to use custom messages in the renderer


core :: forall d c n. C.Command d c n -> Either Message (C.Command d c n)
core = Right


my :: forall d c n. Message -> Either Message (C.Command d c n)
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
        [ H.id_ "network" ]
        $
            [ H.text name
            , H.div
                [ H.onClick $ H.always_ $ core C.Bang
                ]
                [ H.text "Send" ]
            , H.div
                [ ]
                -- [ H.onClick $ H.always_ $ core
                --     $ C.AddInlet (P.toNode "test" "random") "test" ]
                [ H.text "Add Inlet" ]
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
                [ H.classes [ "patch" ] ]
                $ [ H.text name ] <>
                    (viewNode toolkitRenderer pushMsg ui nw
                                <$> (nodes # Seq.toUnfoldable))
        _ ->
            H.div
                [ H.classes [ "patch" ] ]
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
                [ H.classes [ "node" ] ]
                (
                    [ H.text name
                    , toolkitRenderer.renderNode
                            n
                            node
                            (case pushMsg of R.PushF f -> f)
                    ]
                    <> (viewInlet toolkitRenderer pushMsg ui nw
                                <$> (inlets # Seq.toUnfoldable))
                    <> (viewOutlet toolkitRenderer pushMsg ui nw
                                <$> (outlets # Seq.toUnfoldable))
                )
        _ -> H.div
                [ H.classes [ "node" ] ]
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
                [ H.classes [ "inlet" ] ]
                [ H.text label
                , toolkitResult inlet channel
                , case Map.lookup path ui.lastInletData of
                    Just d -> H.text "data"
                    _ -> H.text ""
                ]
        _ -> H.div
                [ H.classes [ "inlet" ] ]
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
                [ H.classes [ "outlet" ]
                ]
                [ H.text label
                , toolkitResult outlet channel
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
    -> Either Message (C.Command d c n)
    -> Maybe (DebugBox.Model d c n)
    -> Maybe (DebugBox.Model d c n)
updateDebugBox nw (Right cmd) (Just debug) = Just $ DebugBox.update cmd nw debug
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
            \cmdOrMsg (ui /\ nw) ->
                let
                    (ui' /\ cmds) = update cmdOrMsg (ui /\ nw)
                    ui'' =
                        ui' { debug = ui'.debug # updateDebugBox nw cmdOrMsg }
                in (ui'' /\ cmds)
        , view : view toolkitRenderer
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
    viewError err


update
    :: forall d c n
     . Either Message (C.Command d c n)
    -> Model d c n /\ R.Network d c n
    -> Model d c n /\ Array (Either Message (C.Command d c n))
update (Right C.Bang) (ui /\ _) = ui /\ []
update (Right (C.GotInletData inletPath d)) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update (Right (C.GotOutletData outletPath d)) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update (Right (C.AddInlet nodePath alias c)) ( ui /\ nw ) =
    ( ui /\ [] )
update _ (ui /\ _) = ui /\ []

