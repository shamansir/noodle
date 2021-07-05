module App.Component.Network where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple

import Noodle.Network (Network) as Noodle
import Noodle.Network as Network

import App.Colors as Colors
import App.Component.Patch as PatchC


import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA


type Slots = ( patch :: PatchC.Slot Int )


_patch = Proxy :: Proxy "patch"


type Input d =
    { nw :: Noodle.Network d Unit
    }


type State d =
    { nw :: Noodle.Network d Unit
    , currentPatch :: Maybe String
    , width :: Number, height :: Number
    }


data Action d
    = SelectPatch String
    | HandlePatch (PatchC.Action d)


initialState :: forall d. Input d -> State d
initialState { nw } = { nw, currentPatch : Just "base", width : 1000.0, height : 1000.0 }


render :: forall d m. State d -> H.ComponentHTML (Action d) Slots m
render { nw, currentPatch, width, height } =
    HS.svg
        [ HSA.width width, HSA.height height ]
        [ background
        , patchesTabs
        , maybeCurrent $ (flip Network.patch $ nw) =<< currentPatch
        ]
    where
        tabHeight = 20.0
        tabLength = 60.0
        background =
            HS.rect [ HSA.width width, HSA.height height, HSA.fill $ Just Colors.background ]
        patchesTabs = HS.g [ HSA.class_ $ H.ClassName "patches-tabs" ] (patchLabel <$> Tuple.fst <$> Network.patches nw)
        patchLabel label =
            HS.g
                [ HSA.class_ $ H.ClassName "patch-tab" ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight, HSA.fill $ Just Colors.tabBackground ]
                , HS.text [] [ HH.text label ]
                ]
        maybeCurrent (Just patch) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 tabHeight ] ]
                [ HH.slot _patch 0 PatchC.component { patch } absurd ]
        maybeCurrent Nothing =
            HS.text
                [ HSA.transform [ HSA.Translate 0.0 tabHeight ] ]
                [ HH.text "No patch selected" ]


handleAction :: forall output m d. Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of
    SelectPatch _ ->
        H.modify_ \state -> state
    HandlePatch _ ->
        H.modify_ \state -> state


component :: forall query output m d. H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }