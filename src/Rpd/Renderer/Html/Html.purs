module Rpd.Renderer.Html.Html where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Lens (view) as L
import Data.Lens.At (at) as L
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)

import Rpd.API (RpdError) as R
import Rpd.Command (Command(..)) as C
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.Path as P
import Rpd.UUID as UUID
import Rpd.Render as R
import Rpd.Render.MUV (Renderer(..), PushF(..)) as R
import Rpd.Util (type (/->))

import Rpd.Renderer.Html.DebugBox as DebugBox

import Spork.Html (Html)
import Spork.Html as H


type Model d =
    -- TODO: use UUID to store inlets?
    { lastInletData :: P.ToInlet /-> d
    , lastOutletData :: P.ToOutlet /-> d
    , debug :: Maybe DebugBox.Model
    }


data Message
    = NoOp
    | ClickAt (Int /\ Int)
    | EnableDebug
    | DisableDebug


type View d = Html (Either Message (C.Command d))


init :: forall d. Model d
init =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    , debug : Nothing
    }


type HtmlRenderer d = R.Renderer d (Model d) (View d) Message


emptyView :: forall d. View d
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d. R.RpdError -> View d
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork :: forall d. R.PushF Message d -> Model d -> R.Network d -> View d
viewNetwork pushMsg ui nw@(R.Network { name, patches }) =
    H.div
        [ H.id_ "network" ]
        $ [ H.text name ] <>
            (toUnfoldable $ viewPatch pushMsg ui nw <$> (patches  # Set.toUnfoldable))


viewPatch
    :: forall d
     . R.PushF Message d
    -> Model d
    -> R.Network d
    -> UUID.ToPatch
    -> View d
viewPatch pushMsg ui nw patchUuid =
    case L.view (L._patch patchUuid) nw of
        Just (R.Patch _ (P.ToPatch name) nodes) ->
            H.div
                [ H.classes [ "patch" ] ]
                $ [ H.text name ] <>
                    (viewNode pushMsg ui nw <$> (nodes # Set.toUnfoldable))
        _ ->
            H.div
                [ H.classes [ "patch" ] ]
                [ H.text $ "patch " <> show patchUuid <> " was not found" ]

viewNode
    :: forall d
     . R.PushF Message d
    -> Model d
    -> R.Network d
    -> UUID.ToNode
    -> View d
viewNode pushMsg ui nw nodeUuid =
    case L.view (L._node nodeUuid) nw of
        Just (R.Node _ (P.ToNode { node : name }) _ { inlets, outlets }) ->
            H.div
                [ H.classes [ "node" ] ]
                (
                    [ H.text name
                    , H.div
                        [ H.onClick $ H.always_ $ Right
                            -- $ FIXME: C.SendToInlet (P.toInlet "0" "0" "0") R.default ]
                            $ C.Bang ]
                        [ H.text "Send" ]
                    , H.div
                        [ H.onClick $ H.always_ $ Right
                            $ C.AddInlet (P.toInlet "0" "0" "0") ]
                        [ H.text "Add Inlet" ]
                    ]
                    <> (viewInlet pushMsg ui nw <$> (inlets # Set.toUnfoldable))
                    <> (viewOutlet pushMsg ui nw <$> (outlets # Set.toUnfoldable))
                )
        _ -> H.div
                [ H.classes [ "node" ] ]
                [ H.text $ "node " <> show nodeUuid <> " was not found" ]


viewInlet
    :: forall d
     . R.PushF Message d
    -> Model d
    -> R.Network d
    -> UUID.ToInlet
    -> View d
viewInlet pushMsg ui nw inletUuid =
    case L.view (L._inlet inletUuid) nw of
        Just (R.Inlet _ path@(P.ToInlet { inlet : label }) { flow }) ->
            H.div
                [ H.classes [ "inlet" ] ]
                [ H.text label
                , case Map.lookup path ui.lastInletData of
                    Just d -> H.text "data"
                    _ -> H.text ""
                ]
        _ -> H.div
                [ H.classes [ "inlet" ] ]
                [ H.text $ "inlet " <> show inletUuid <> " was not found" ]


viewOutlet
    :: forall d
     . R.PushF Message d
    -> Model d
    -> R.Network d
    -> UUID.ToOutlet
    -> View d
viewOutlet pushMsg ui nw outletUuid =
    case L.view (L._outlet outletUuid) nw of
        Just (R.Outlet _ path@(P.ToOutlet { outlet : label }) { flow }) ->
            H.div
                [ H.classes [ "outlet" ] ]
                $ [ H.text label ]
        _ -> H.div
                [ H.classes [ "outlet" ] ]
                [ H.text $ "outlet " <> show outletUuid <> " was not found" ]


viewDebugWindow
    :: forall d
     . R.PushF Message d
    -> Model d
    -> R.Network d
    -> View d
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


htmlRenderer :: forall d. HtmlRenderer d
htmlRenderer =
    R.Renderer
        { from : emptyView
        , init
        , update
        , view
        }


view
    :: forall d
     . R.PushF Message d
    -> Either R.RpdError (Model d /\ R.Network d)
    -> View d
view pushMsg (Right (ui /\ nw)) =
    H.div [ ]
        [ viewDebugWindow pushMsg ui nw
        , viewNetwork pushMsg ui nw
        ]
view pushMsg (Left err) =
    viewError err


update
    :: forall d
     . Either Message (C.Command d)
    -> Model d /\ R.Network d
    -> Model d /\ Array (Either Message (C.Command d))
update cmdOrMsg (ui /\ nw) =
    let
        ui' =
            case ( ui.debug /\ cmdOrMsg ) of
                ( Just debug /\ Right cmd ) ->
                    ui
                        { debug = Just $ DebugBox.update cmd nw debug
                        }
                _ -> ui
    in update' cmdOrMsg (ui /\ nw)


update'
    :: forall d
     . Either Message (C.Command d)
    -> Model d /\ R.Network d
    -> Model d /\ Array (Either Message (C.Command d))
update' (Right C.Bang) (ui /\ _) = ui /\ []
update' (Right (C.GotInletData inletPath d)) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update' (Right (C.GotOutletData outletPath d)) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update' _ (ui /\ _) = ui /\ []

