module Hydra.Component.Background where


import Prelude

import App.Toolkit.UI (BgInput)

import Effect.Class (class MonadEffect, liftEffect)

import Data.Maybe

import Hydra (Hydra)
import Hydra.Engine as Hydra

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA


type State =
    { hydraReady :: Boolean
    }


data Action =
    Initialize


canvasId :: String
canvasId = "hydra-canvas"


initialState :: BgInput Hydra -> State
initialState _ =
    { hydraReady : false }


render :: forall m. State -> H.ComponentHTML Action () m
render _ =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width 1000.0, HSA.height 1000.0
        --, HP.style ""
        -- , HP.style { zIndex: -10000.0, position: absolute }
        -- , HSA.style
        , HSA.id "hydra-holder"
        ]
        [
            HH.canvas
                [ HP.id canvasId
                , HP.style "width: 100%; height: 100%;background-color: black;"
                ]
        ]


handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction Initialize = do
    _ <- liftEffect $ Hydra.init canvasId
    H.modify_ $ _ { hydraReady = true }


component :: forall query output m. MonadEffect m => H.Component query (BgInput Hydra) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }