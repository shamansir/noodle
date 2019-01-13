module Rpd.Renderer.Html.Html where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map as Map
import Data.Set as Set
import Data.Foldable (foldr)
import Data.List (toUnfoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Lens (view) as L
import Data.Lens.At (at) as L

import Rpd.Path as R
import Rpd.Optics as R
import Rpd.API (RpdError) as R
import Rpd.Network as R
import Rpd.Render as R
import Rpd.Command (Command(..)) as C
import Rpd.RenderMUV (Renderer(..), PushMsg, Message(..)) as R

import Spork.Html (Html)
import Spork.Html as H


type Model =
    {
    }


data Msg
    = ClickAt (Int /\ Int)


type Message d = R.Message d Msg


type PushMsg d = R.PushMsg d Msg


type View = Html Msg


init :: Model
init =
    {
    }


type HtmlRenderer d = R.Renderer d Model View Msg


emptyView :: View
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: R.RpdError -> View
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork :: forall d. Model -> R.Network d -> View
viewNetwork ui nw@(R.Network { name } { patches }) =
    H.div
        [ H.id_ "network" ]
        $ [ H.text name ] <>
            (toUnfoldable $ viewPatch ui nw <$> Map.values patches)


viewPatch :: forall d. Model -> R.Network d -> R.Patch d ->  View
viewPatch ui nw (R.Patch patchId { name } { nodes }) =
    H.div
        [ H.classes [ "patch" ] ]
        $ [ H.text name ] <>
            (viewNode ui nw <$> (nodes # Set.toUnfoldable))


viewNode :: forall d. Model -> R.Network d -> R.NodePath -> View
viewNode ui nw nodePath =
    case L.view (R._node nodePath) nw of
        Just (R.Node _ { name } { inlets, outlets }) ->
            H.div
                [ H.classes [ "node" ] ]
                $ [ H.text name ]
                    <> (viewInlet ui nw <$> (inlets # Set.toUnfoldable))
                    <> (viewOutlet ui nw <$> (outlets # Set.toUnfoldable))
        _ -> H.div
                [ H.classes [ "node" ] ]
                [ H.text $ "node " <> show nodePath <> " was not found" ]


viewInlet :: forall d. Model -> R.Network d -> R.InletPath -> View
viewInlet ui nw inletPath =
    case L.view (R._inlet inletPath) nw of
        Just (R.Inlet _ { label } { flow }) ->
            H.div
                [ H.classes [ "inlet" ] ]
                $ [ H.text label ]
        _ -> H.div
                [ H.classes [ "inlet" ] ]
                [ H.text $ "inlet " <> show inletPath <> " was not found" ]


viewOutlet :: forall d. Model -> R.Network d -> R.OutletPath -> View
viewOutlet ui nw outletPath =
    case L.view (R._outlet outletPath) nw of
        Just (R.Outlet _ { label } { flow }) ->
            H.div
                [ H.classes [ "outlet" ] ]
                $ [ H.text label ]
        _ -> H.div
                [ H.classes [ "outlet" ] ]
                [ H.text $ "outlet " <> show outletPath <> " was not found" ]


htmlRenderer :: forall d. HtmlRenderer d
htmlRenderer =
    R.Renderer
        { from : emptyView
        , init : init
        , update
        , view
        }


view :: forall d. PushMsg d -> Either R.RpdError (Model /\ R.Network d) -> View
view pushMsg (Right (ui /\ nw)) =
    viewNetwork ui nw
view pushMsg (Left err) =
    viewError err


update :: forall d. Message d -> (Model /\ R.Network d) -> (Model /\ Array (Message d))
update (R.Core C.Bang) (ui /\ _) = ui /\ []
update _ (ui /\ _) = ui /\ []
