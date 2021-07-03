module App.Component.Network where

import Prelude (Void)

import Type.Proxy (Proxy(..))

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))

import Noodle.Network as Noodle
import App.Component.Patch as PatchC


import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS


type Slots = ( patch :: forall query. H.Slot query Void Int )


_patch = Proxy :: Proxy "patch"


type Input d =
    { nw :: Noodle.Network d Unit
    }


type State d =
    { nw :: Noodle.Network d Unit
    , currentPatch :: Maybe String
    }


data Action
    = SelectPatch String


initialState :: forall d. Input d -> State d
initialState { nw } = { nw, currentPatch : Nothing }


render :: forall d m. State d -> H.ComponentHTML Action Slots m
render _ = HS.svg [] []


handleAction :: forall output m d. Action -> H.HalogenM (State d) Action Slots output m Unit
handleAction = case _ of
  SelectPatch _ ->
    H.modify_ \state -> state


component :: forall query output m d. H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }