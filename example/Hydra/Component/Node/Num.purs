module Hydra.Component.Node.Num where


import Prelude

-- import Data.String.Read (read)
--import Data.Parse
import Data.Number as Number
import Data.Maybe (maybe)

import App.UI (BgInput)
import App.UI as UI

import Hydra (Hydra)
import Hydra as Hydra

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS


type State = Unit


data Action
    = NoOp
    | Change Number


initialState :: UI.NodeInput Hydra -> State
initialState = const unit


render :: forall m. State -> H.ComponentHTML Action () m
render _ =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width 52.0, HSA.height 22.0 ]
        [ HH.input
            [ HP.type_ I.InputNumber
            , HP.width 40, HP.height 9
            , HP.min 0.0
            , HP.max 255.0
            , HP.step $ I.Step 0.1
            , HE.onValueInput (Number.fromString >>> maybe NoOp Change)
            ]
        ]


handleAction :: forall m. Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = case _ of
    Change n ->
        H.raise $ UI.SendToOutlet "num" $ Hydra.num n
    NoOp ->
        pure unit


component :: forall m. UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }