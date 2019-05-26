module Rpd.Renderer.Html.DebugBox
    ( Model, init, update, view )
    where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List, (:))
import Data.List as List

import Spork.Html (Html)
import Spork.Html as H

import Rpd.Command as C
import Rpd.Network as R


type Model d =
    { lastCommands :: List (C.Command d)
    }


init :: forall d. Model d
init =
    { lastCommands : List.Nil
    }


update :: forall d. C.Command d -> R.Network d -> Model d -> Model d
update cmd nw model =
    model
        { lastCommands =
            cmd :
                (if List.length model.lastCommands < 5 then
                    model.lastCommands
                else
                    List.tail model.lastCommands # fromMaybe List.Nil
                )
        }


viewNetwork :: forall d. R.Network d -> Html Unit
viewNetwork nw =
    H.div [ H.classes [ "network-debug" ] ]
        []
    where
        viewPatch :: Maybe (R.Patch d) -> Html Unit
        viewPatch (Just patch) = H.div [] []
        viewPatch _ = H.div [] []
        viewNode :: Maybe (R.Node d) -> Html Unit
        viewNode (Just node) = H.div [] []
        viewNode _ = H.div [] []
        viewInlet :: Maybe (R.Inlet d) -> Html Unit
        viewInlet (Just inlet) = H.div [] []
        viewInlet _ = H.div [] []
        viewOutlet :: Maybe (R.Outlet d) -> Html Unit
        viewOutlet (Just outlet) = H.div [] []
        viewOutlet _ = H.div [] []
        viewLink :: Maybe R.Link -> Html Unit
        viewLink (Just link) = H.div [] []
        viewLink _ = H.div [] []


viewModel :: forall d. Show d => Model d -> Html Unit
viewModel model =
    H.ul [ H.classes [ "commands-debug" ] ]
        $ List.toUnfoldable (viewCommand <$> model.lastCommands)
    where
        viewCommand :: C.Command d -> Html Unit
        viewCommand cmd =
            H.li [] [ H.text $ show cmd ]


view :: forall d. Show d => R.Network d -> Model d -> Html Unit
view nw model =
    H.div [ H.id_ "debug" ]
        [ H.div [ H.classes [ "network-debug" ] ]
            [ viewNetwork nw
            , viewModel model
            ]
        ]
