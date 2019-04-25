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

import Rpd.Util (type (/->))
import Rpd.Path as R
import Rpd.Optics as R
import Rpd.API (RpdError) as R
import Rpd.Network as R
import Rpd.Render as R
import Rpd.Command (Command(..)) as C
import Rpd.Render.MUV (Renderer(..), PushMsg, Message(..)) as R

import Spork.Html (Html)
import Spork.Html as H


type Model d =
    { lastInletData :: R.InletPath /-> d
    , lastOutletData :: R.OutletPath /-> d
    }


data HtmlMsg
    = ClickAt (Int /\ Int)


type AnyMessage d = R.Message d HtmlMsg


type PushAnyMsg d = R.PushMsg d HtmlMsg


type View d = Html (AnyMessage d)


init :: forall d. Model d
init =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    }


type HtmlRenderer d = R.Renderer d (Model d) (View d) (AnyMessage d)


emptyView :: forall d. View d
emptyView = H.div [ H.id_ "network" ] [ H.text "empty" ]


viewError :: forall d. R.RpdError -> View d
viewError error =
    H.div [ H.id_ "error" ] [ H.text $ show error ]


viewNetwork :: forall d. Model d -> R.Network d -> View d
viewNetwork ui nw@(R.Network { name } { patches }) =
    H.div
        [ H.id_ "network" ]
        $ [ H.text name ] <>
            (toUnfoldable $ viewPatch ui nw <$> Map.values patches)


viewPatch :: forall d. Model d -> R.Network d -> R.Patch d ->  View d
viewPatch ui nw (R.Patch patchId { name } { nodes }) =
    H.div
        [ H.classes [ "patch" ] ]
        $ [ H.text name ] <>
            (viewNode ui nw <$> (nodes # Set.toUnfoldable))


viewNode :: forall d. Model d -> R.Network d -> R.NodePath -> View d
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


viewInlet :: forall d. Model d -> R.Network d -> R.InletPath -> View d
viewInlet ui nw inletPath =
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


viewOutlet :: forall d. Model d -> R.Network d -> R.OutletPath -> View d
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
        , init
        , update : ?wh
        , view : ?wh
        }


view
    :: forall d
     . R.PushMsg d (R.Message d HtmlMsg)
    -> Either R.RpdError (Model d /\ R.Network d)
    -> Html (R.Message d HtmlMsg)
view pushMsg (Right (ui /\ nw)) =
    viewNetwork ui nw
view pushMsg (Left err) =
    viewError err


update
    :: forall d
     . R.Message d HtmlMsg
    -> (Model d /\ R.Network d)
    -> (Model d /\ Array (R.Message d HtmlMsg))
update (R.Core C.Bang) (ui /\ _) = ui /\ []
update (R.GotInletData inletPath d) (ui /\ _) =
    (ui { lastInletData = ui.lastInletData # Map.insert inletPath d })
    /\ []
update (R.GotOutletData outletPath d) (ui /\ _) =
    (ui { lastOutletData = ui.lastOutletData # Map.insert outletPath d })
    /\ []
update _ (ui /\ _) = ui /\ []


class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Self m = Self m

instance selfAction :: Monoid m => Action m (Self m) where
  act m (Self a) = Self (m <> a)

