module Hydra.Component.Input where


import Prelude (($), (>>>), show, const, Unit, unit)

import Data.Number as Number
import Data.Maybe (maybe)

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS


number :: forall w action. Number -> { min :: Number, max :: Number, step :: Number } -> action -> (Number -> action) -> HH.HTML w action
number val { min, max, step } def handler =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width 52.0, HSA.height 22.0 ]
        [ HH.input
            [ HP.type_ I.InputNumber
            , HP.width 40, HP.height 9
            , HP.min min
            , HP.max max
            , HP.step $ I.Step step
            , HP.value $ show val
            , HE.onValueInput (Number.fromString >>> maybe def handler)
            ]
        ]


button :: forall w action. action -> HH.HTML w action
button handler =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width 52.0, HSA.height 22.0 ]
        [ HH.input
            [ HP.type_ I.InputButton
            , HP.width 40, HP.height 9
            , HE.onClick $ const $ handler
            ]
        ]