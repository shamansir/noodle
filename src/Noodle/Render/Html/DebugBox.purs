module Noodle.Render.Html.DebugBox
    ( debugBox, DebugBox, Action, Model, Filter
    , init, update, view
    , ActionsKind, FilterState )
    where


import Prelude

import Data.String as String
import Data.Either (Either(..))
import Data.Lens (view) as L
import Data.List (List, (:))
import Data.List (foldr, length, toUnfoldable, take, List(..)) as List
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\), type (/\))

import Spork.Html (Html)
import Spork.Html as H

import Noodle.API.Action as Core
import Noodle.Network as R
import Noodle.Optics as L
import Noodle.Process as R
import Noodle.Render.Atom as R
import Noodle.UUID as R

import UI (UI)
import UI (makeMinimal) as UI


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


instance showActionsKind :: Show ActionsKind where
    show Build = "Build"
    show Inner = "Inner"
    show Data = "Data"
    show Request = "Request"


derive instance eqActionsKind :: Eq ActionsKind


data FilterState = On | Off


type Filter = List (ActionsKind /\ FilterState)


type Model d c n =
    { lastActions :: List (Core.Action d c n)
    , filter :: Filter
    }


init :: forall d c n. Model d c n
init =
    { lastActions : List.Nil
    , filter
        : (Build /\ On)
        : (Data /\ Off)
        : (Request /\ Off)
        : (Inner /\ Off)
        : List.Nil
    }


update
    :: forall d c n
     . Either Action (Core.Action d c n)
    -> R.Network d c n /\ Model d c n
    -> Model d c n
update (Right action) (nw /\ model) =
    model
        { lastActions =
            if (not $ filtered)
            then
                action :
                    (if List.length model.lastActions < actionStackSize then
                        model.lastActions
                    else
                        List.take actionStackSize model.lastActions
                    )
            else model.lastActions
        }
    where
        filtered = List.foldr check true model.filter
        check (kind /\ state) prev =
            prev && case action /\ kind /\ state of
                Core.Build _   /\ Build   /\ On -> false
                Core.Data _    /\ Data    /\ On -> false
                Core.Inner _   /\ Inner   /\ On -> false
                Core.Request _ /\ Request /\ On -> false
                _ -> true

update (Left (Invert kind)) (nw /\ model) =
    model
        { filter = switchIfKind <$> model.filter
        }
    where
        switchIfKind (otherKind /\ On)  | otherKind == kind = otherKind /\ Off
        switchIfKind (otherKind /\ Off) | otherKind == kind = otherKind /\ On
        switchIfKind (otherKind /\ val) | otherwise         = otherKind /\ val


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
    H.div
        [ ]
        [ H.ul [ H.classes [ "filter-controls" ] ]
            $ List.toUnfoldable (filterSwitch <$> model.filter)
        , H.ul [ H.classes [ "commands-debug" ] ]
            $ List.toUnfoldable (viewAction <$> model.lastActions)
        ]
    where
        filterSwitch :: ActionsKind /\ FilterState -> Html Action
        filterSwitch (kind /\ filterState) =
            H.li
                []
                [ H.label [ H.for checkboxId ] [ H.text $ show kind ]
                , H.input
                    [ H.type_ H.InputCheckbox
                    , H.id_ checkboxId
                    , H.checked $ case filterState of
                        On -> true
                        Off -> false
                    , H.onValueChange $ const $ Just $ Invert kind
                    ]
                ]
            where checkboxId = "filter-dbox-" <> (String.toLower $ show kind)
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
