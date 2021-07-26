module Hydra.Component.Background where


import Prelude (Unit, unit, const, pure)

import App.UI (BgInput)

import Hydra (Hydra)

import Halogen as H
import Halogen.HTML as HH


type State = Unit


type Action = Unit


initialState :: BgInput Hydra -> State
initialState = const unit


render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.canvas []


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction _ = pure unit


component :: forall query output m. H.Component query (BgInput Hydra) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }