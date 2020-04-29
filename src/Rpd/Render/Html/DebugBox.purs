module Rpd.Render.Html.DebugBox
    ( debugBox, DebugBox, Action, Model, Filter
    , init, update, view )
    where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as List
import Data.Sequence as Seq
import Data.Lens (view) as L
import Data.Tuple.Nested ((/\), type (/\))

import Spork.Html (Html)
import Spork.Html as H

import UI (UI)
import UI (makeMinimal) as UI

import Rpd.API.Action as Core
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.UUID as R
import Rpd.Process as R

import Rpd.Render.Atom as R


actionStackSize :: Int
actionStackSize = 10


type DebugBox d c n =
    UI
        (Either Action (Core.Action d c n))
        (R.Network d c n /\ Model d c n)
        (Html Action)


debugBox
    :: forall d c n
     . Show d => Show c => Show n
    => DebugBox d c n
debugBox =
    UI.makeMinimal
        (\action (nw /\ model) -> nw /\ update action (nw /\ model))
        view


data Action
    = Invert ActionsKind


data ActionsKind
    = Build
    | Data
    | Inner
    | Request


allKinds :: List ActionsKind
allKinds = Build : Data : Inner : List.Nil


data FilterState = On | Off


data Filter =
    Filter (List (ActionsKind /\ FilterState))


type Model d c n =
    { lastActions :: List (Core.Action d c n)
    , filter :: Filter
    }


init :: forall d c n. Model d c n
init =
    { lastActions : List.Nil
    , filter : Filter $ (flip (/\) Off) <$> allKinds
    }


update
    :: forall d c n
     . Either Action (Core.Action d c n)
    -> R.Network d c n /\ Model d c n
    -> Model d c n
update (Right action) (nw /\ model) =
    model
        { lastActions =
            action :
                (if List.length model.lastActions < actionStackSize then
                    model.lastActions
                else
                    List.take actionStackSize model.lastActions
                )
        }
update (Left msg) (nw /\ model) =
    model


-- viewItems
--     :: forall uuid x a
--      . (Maybe x -> Html Msg)
--     -> (uuid -> L.Lens' a (Maybe x))
--     -> Set uuid
--     -> a
--     -> Array (Html Msg)
-- viewItems viewItem lens items nw =
--     viewItem
--         <$> (\uuid -> L.view (lens uuid) nw)
--         <$> (Set.toUnfoldable items :: Array uuid)


viewNetwork :: forall d c n. R.Network d c n -> Html Action
viewNetwork nw@(R.Network { patches, name }) =
    H.div [ H.classes [ "network-debug" ] ]
        [ H.div [] [ H.text name ]
        , H.ul [] viewPatches
        -- [ H.ul [] (viewItems viewPatch ?wh patches nw)
        ]
    where
        viewPatches =
            viewPatch
                <$> (\patchUuid -> L.view (L._patch patchUuid) nw)
                <$> (Seq.toUnfoldable patches :: Array R.ToPatch)
        viewNodes nodes =
            viewNode
                <$> (\nodeUuid -> L.view (L._node nodeUuid) nw)
                <$> (Seq.toUnfoldable nodes :: Array R.ToNode)
        viewInlets inlets =
            viewInlet
                <$> (\inletUuid -> L.view (L._inlet inletUuid) nw)
                <$> (Seq.toUnfoldable inlets :: Array R.ToInlet)
        viewOutlets outlets =
            viewOutlet
                <$> (\outletUuid -> L.view (L._outlet outletUuid) nw)
                <$> (Seq.toUnfoldable outlets :: Array R.ToOutlet)
        viewLinks links =
            viewLink
                <$> (\linkUuid -> L.view (L._link linkUuid) nw)
                <$> (Seq.toUnfoldable links :: Array R.ToLink)
        viewPatch :: Maybe (R.Patch d c n) -> Html Action
        viewPatch (Just (R.Patch uuid path { nodes, links })) =
            H.li [ H.classes [ "patch-debug" ] ]
                [ H.div []
                    [ H.span [] [ H.text $ show uuid ]
                    , H.span [] [ H.text $ show path ]
                    , H.ul [] $ viewNodes nodes
                    , H.ul [] $ viewLinks links
                    ]
                ]
        viewPatch _ =
            H.li [ H.classes [ "patch-debug" ] ]
                [ H.text "Unknown patch" ]
        viewNode :: Maybe (R.Node d n) -> Html Action
        viewNode (Just (R.Node uuid path n processF { inlets, outlets })) =
            H.li [ H.classes [ "node-debug" ] ]
                [ H.div []
                    [ H.span [] [ H.text $ show uuid ]
                    , H.span [] [ H.text $ show path ]
                    , H.span []
                        [ H.text $ case processF of
                            R.Withhold -> "withhold"
                            R.Process _ -> "process"
                            R.ProcessST _ -> "process-w/state"
                        ]
                    , H.ul [] $ viewInlets inlets
                    , H.ul [] $ viewOutlets outlets
                    ]
                ]
        viewNode _ =
            H.li [ H.classes [ "node-debug" ] ]
                [ H.text "Unknown node" ]
        viewInlet :: Maybe (R.Inlet d c) -> Html Action
        viewInlet (Just (R.Inlet uuid path c _)) =
            H.li  [ H.classes [ "inlet-debug" ] ]
                [ H.span [] [ H.text $ show uuid ]
                , H.span [] [ H.text $ show path ]
                ]
        viewInlet _ =
            H.li [ H.classes [ "inlet-debug" ] ]
                [ H.text "Unknown inlet" ]
        viewOutlet :: Maybe (R.Outlet d c) -> Html Action
        viewOutlet (Just (R.Outlet uuid path c _)) =
            H.li  [ H.classes [ "outlet-debug" ] ]
                [ H.span [] [ H.text $ show uuid ]
                , H.span [] [ H.text $ show path ]
                ]
        viewOutlet _ =
            H.li [ H.classes [ "outlet-debug" ] ]
                [ H.text "Unknown outlet" ]
        viewLink :: Maybe R.Link -> Html Action
        viewLink (Just (R.Link uuid { outlet, inlet })) =
            H.li  [ H.classes [ "link-debug" ] ]
                [ H.span [] [ H.text $ show uuid ]
                , H.span [] [ H.text $ show outlet ]
                , H.span [] [ H.text $ show inlet ]
                ]
        viewLink _ =
            H.li [ H.classes [ "link-debug" ] ]
                [ H.text "Unknown link" ]


viewModel
    :: forall d c n
     . Show d => Show c => Show n
    => Model d c n
    -> Html Action
viewModel model =
    H.ul [ H.classes [ "commands-debug" ] ]
        $ List.toUnfoldable (viewAction <$> model.lastActions)
    where
        viewAction :: Core.Action d c n -> Html Action
        viewAction action =
            H.li [] [ H.text $ show action ]


view
    :: forall d c n
     . Show d => Show c => Show n
    => R.Network d c n /\ Model d c n
    -> Html Action
view (nw /\ model) =
    H.div [ ]
        [ viewNetwork nw
        , viewModel model
        ]
