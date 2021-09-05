module Hydra.Component.Background where


import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Color.Extra as C

import Data.Maybe
import Data.Vec2 (Size)
import Data.Vec2 as V2

import Hydra (Hydra)
import Hydra.Engine as Hydra

import App.Style.Hydra.Background (bg)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Hydra.Toolkit.UI.Components as UI



type State =
    { hydraReady :: Boolean
    , size :: Size
    }


data Action
    = Initialize
    | Receive UI.BgInput


canvasId :: String
canvasId = "hydra-canvas"


initialState :: UI.BgInput -> State
initialState _ =
    { hydraReady : false, size : zero }


render :: forall m. State -> H.ComponentHTML Action () m
render { size } =
    HS.foreignObject
        [ HSA.x 0.0, HSA.y 0.0, HSA.width $ V2.w size, HSA.height $ V2.h size
        , HSA.id "hydra-holder"
        ]
        [
            HH.canvas
                [ HP.id canvasId
                , HP.style $ "width: 100%; height: 100%;background-color: " <> (HSA.printColor $ Just $ C.toSvg bg.fill) <> ";"
                ]
        ]


handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction Initialize = do
    _ <- liftEffect $ Hydra.init canvasId
    H.modify_ $ _ { hydraReady = true }
handleAction (Receive { size }) =
    H.modify_ $ _ { size = size }


component :: forall m. MonadEffect m => UI.BgComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }