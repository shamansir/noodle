module Rpd.Renderer.Html.DebugBox
    ( Model, init, update, view )
    where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List, (:))
import Data.List as List
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Lens (view, Lens') as L

import Spork.Html (Html)
import Spork.Html as H

import Rpd.API.Action as A
import Rpd.Network as R
import Rpd.Optics as L
import Rpd.UUID as R
import Rpd.Process as R


actionStackSize :: Int
actionStackSize = 10


type Model d c n =
    { lastActions :: List (A.Action d c n)
    }


init :: forall d c n. Model d c n
init =
    { lastActions : List.Nil
    }


update
    :: forall d c n
     . A.Action d c n
    -> R.Network d c n
    -> Model d c n
    -> Model d c n
update cmd nw model =
    model
        { lastActions =
            cmd :
                (if List.length model.lastActions < actionStackSize then
                    model.lastActions
                else
                    List.take actionStackSize model.lastActions
                )
        }


-- viewItems
--     :: forall uuid x a
--      . (Maybe x -> Html Unit)
--     -> (uuid -> L.Lens' a (Maybe x))
--     -> Set uuid
--     -> a
--     -> Array (Html Unit)
-- viewItems viewItem lens items nw =
--     viewItem
--         <$> (\uuid -> L.view (lens uuid) nw)
--         <$> (Set.toUnfoldable items :: Array uuid)


viewNetwork :: forall d c n. R.Network d c n -> Html Unit
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
        viewPatch :: Maybe (R.Patch d c n) -> Html Unit
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
        viewNode :: Maybe (R.Node d n) -> Html Unit
        viewNode (Just (R.Node uuid path n processF { inlets, outlets })) =
            H.li [ H.classes [ "node-debug" ] ]
                [ H.div []
                    [ H.span [] [ H.text $ show uuid ]
                    , H.span [] [ H.text $ show path ]
                    , H.span []
                        [ H.text $ case processF of
                            R.Withhold -> "withhold"
                            R.Process _ -> "process"
                            R.Process' _ -> "process-sign"
                        ]
                    , H.ul [] $ viewInlets inlets
                    , H.ul [] $ viewOutlets outlets
                    ]
                ]
        viewNode _ =
            H.li [ H.classes [ "node-debug" ] ]
                [ H.text "Unknown node" ]
        viewInlet :: Maybe (R.Inlet d c) -> Html Unit
        viewInlet (Just (R.Inlet uuid path c _)) =
            H.li  [ H.classes [ "inlet-debug" ] ]
                [ H.span [] [ H.text $ show uuid ]
                , H.span [] [ H.text $ show path ]
                ]
        viewInlet _ =
            H.li [ H.classes [ "inlet-debug" ] ]
                [ H.text "Unknown inlet" ]
        viewOutlet :: Maybe (R.Outlet d c) -> Html Unit
        viewOutlet (Just (R.Outlet uuid path c _)) =
            H.li  [ H.classes [ "outlet-debug" ] ]
                [ H.span [] [ H.text $ show uuid ]
                , H.span [] [ H.text $ show path ]
                ]
        viewOutlet _ =
            H.li [ H.classes [ "outlet-debug" ] ]
                [ H.text "Unknown outlet" ]
        viewLink :: Maybe R.Link -> Html Unit
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
    -> Html Unit
viewModel model =
    H.ul [ H.classes [ "commands-debug" ] ]
        $ List.toUnfoldable (viewAction <$> model.lastActions)
    where
        viewAction :: A.Action d c n -> Html Unit
        viewAction action =
            H.li [] [ H.text $ show action ]

view
    :: forall d c n
     . Show d => Show c => Show n
    => R.Network d c n
    -> Model d c n
    -> Html Unit
view nw model =
    H.div [ ]
        [ viewNetwork nw
        , viewModel model
        ]
