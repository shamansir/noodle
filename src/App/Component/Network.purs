module App.Component.Network where

import Prelude (Void, ($), (<$>), (<*>), flip, absurd, (=<<))

import Type.Proxy (Proxy(..))

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple

import Noodle.Network (Network) as Noodle
import Noodle.Network as Network
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
    }


data Action d
    = SelectPatch String
    | HandlePatch (PatchC.Action d)


initialState :: forall d. Input d -> State d
initialState { nw } = { nw, currentPatch : Nothing }


render :: forall d m. State d -> H.ComponentHTML (Action d) Slots m
render { nw, currentPatch } =
    HS.svg
        [ HSA.width 300.0, HSA.height 400.0 ]
        [ patchesTabs
        , maybeCurrent $ (flip Network.patch $ nw) =<< currentPatch
        ]
    where
        patchesTabs = HS.g [] (patchLabel <$> Tuple.fst <$> Network.patches nw)
        patchLabel label = HS.text [] [ HH.text label ]
        maybeCurrent (Just patch) = HH.slot _patch 0 PatchC.component { patch } absurd
        maybeCurrent Nothing = HS.text [] [ HH.text "No patch selected" ]


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