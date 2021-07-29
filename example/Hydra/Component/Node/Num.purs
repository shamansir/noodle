module Hydra.Component.Node.Num where


import Prelude (Unit, unit, const, pure)

import App.UI (BgInput)
import App.UI as UI

import Hydra (Hydra)

import DOM.HTML.Indexed.InputType (InputType(..)) as I

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

type State = Unit


type Action = Unit


initialState :: UI.NodeInput Hydra -> State
initialState = const unit


render :: forall m. State -> H.ComponentHTML Action () m
render _ =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width 50.0, HSA.height 10.0 ]
        [ HH.input
            [ HP.type_ I.InputNumber
            , HP.width 50, HP.height 10
            ]
        ]


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction _ = pure unit


component :: forall m. UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }