module Rpd.Renderer.Html.Html where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Lens (view) as L
import Data.Lens.At (at) as L
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Rpd.API (RpdError) as R
import Rpd.Command (Command(..)) as C
import Rpd.Channel (class Channel, default) as R
import Rpd.Network as R
import Rpd.Optics as R
import Rpd.Path as R
import Rpd.Render as R
import Rpd.Render.MUV (Renderer(..), PushF(..)) as R
import Rpd.Util (type (/->))
import Spork.Html (Html)
import Spork.Html as H


type Model d =
    { lastInletData :: R.InletPath /-> d
    , lastOutletData :: R.OutletPath /-> d
    }


data Message
    = ClickAt (Int /\ Int)


type View d = Html (Either Message (C.Command d))


init :: forall d. Model d
init =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    }


type HtmlRenderer d = R.IsData d => R.Renderer d (Model d) (View d) Message


emptyView :: forall d. View d
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d. R.RpdError -> View d
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork :: forall d. R.IsData d => R.PushF Message d -> Model d -> R.Network d -> View d
viewNetwork pushMsg ui nw@(R.Network { name } { patches }) =
    H.div
        [ H.id_ "network" ]
        $ [ H.text name ] <>
            (toUnfoldable $ viewPatch pushMsg ui nw <$> Map.values patches)


viewPatch :: forall d. R.IsData d => R.PushF Message d -> Model d -> R.Network d -> R.Patch d ->  View d
viewPatch pushMsg ui nw (R.Patch patchPath { name } { nodes }) =
    H.div
        [ H.classes [ "patch" ] ]
        $ [ H.text name ] <>
            (viewNode pushMsg ui nw <$> (nodes # Set.toUnfoldable))


testInlet :: forall d. R.IsData d => R.InletDef d
testInlet =
    { accept : Just $ const true
    , default : Just R.default
    , label : "test"
    }


viewNode
    :: forall d
     . R.IsData d
    => R.PushF Message d
    -> Model d
    -> R.Network d
    -> R.NodePath
    -> View d
viewNode pushMsg ui nw nodePath =
    case L.view (R._node nodePath) nw of
        Just (R.Node _ { name } { inlets, outlets }) ->
            H.div
                [ H.classes [ "node" ] ]
                (
                    [ H.text name
                    , H.div
                        [ H.onClick $ H.always_ $ Right $ C.SendToInlet (R.inletPath 0 0 0) R.default ]
                        [ H.text "Send" ]
                    , H.div
                        [ H.onClick $ H.always_ $ Right $ C.AddInlet (R.nodePath 0 0) testInlet ]
                        [ H.text "Add Inlet" ]
                    ]
                    <> (viewInlet pushMsg ui nw <$> (inlets # Set.toUnfoldable))
                    <> (viewOutlet pushMsg ui nw <$> (outlets # Set.toUnfoldable))
                )
        _ -> H.div
                [ H.classes [ "node" ] ]
                [ H.text $ "node " <> show nodePath <> " was not found" ]


viewInlet
    :: forall d
     . R.IsData d
    => R.PushF Message d
    -> Model d
    -> R.Network d
    -> R.InletPath
    -> View d
viewInlet pushMsg ui nw inletPath =
    case L.view (R._inlet inletPath) nw of
        Just (R.Inlet _ { label } { flow }) ->
            H.div
                [ H.classes [ "inlet" ] ]
                [ H.text label
                , case Map.lookup inletPath ui.lastInletData of
                    Just d -> H.text "data"
                    _ -> H.text ""
                ]
        _ -> H.div
                [ H.classes [ "inlet" ] ]
                [ H.text $ "inlet " <> show inletPath <> " was not found" ]


viewOutlet
    :: forall d
     . R.IsData d
    => R.PushF Message d
    -> Model d
    -> R.Network d
    -> R.OutletPath
    -> View d
viewOutlet pushMsg ui nw outletPath =
    case L.view (R._outlet outletPath) nw of
        Just (R.Outlet _ { label } { flow }) ->
            H.div
                [ H.classes [ "outlet" ] ]
                $ [ H.text label ]
        _ -> H.div
                [ H.classes [ "outlet" ] ]
                [ H.text $ "outlet " <> show outletPath <> " was not found" ]


htmlRenderer :: forall d. R.IsData d => HtmlRenderer d
htmlRenderer =
    R.Renderer
        { from : emptyView
        , init
        , update
        , view
        }


view
    :: forall d
     . R.IsData d
    => R.PushF Message d
    -> Either R.RpdError (Model d /\ R.Network d)
    -> View d
view pushMsg (Right (ui /\ nw)) =
    viewNetwork pushMsg ui nw
view pushMsg (Left err) =
    viewError err


update
    :: forall d
     . R.IsData d
    => Either Message (C.Command d)
    -> Model d /\ R.Network d
    -> Model d /\ Array (Either Message (C.Command d))
update (Right C.Bang) (ui /\ _) = ui /\ []
update (Right (C.GotInletData inletPath d)) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update (Right (C.GotOutletData outletPath d)) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update _ (ui /\ _) = ui /\ []

