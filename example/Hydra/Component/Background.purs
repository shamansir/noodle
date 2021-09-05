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

import App.Toolkit.UI (BgInput')



type State =
    { hydraReady :: Boolean
    , size :: Size
    }


data Action patch_state
    = Initialize
    | Receive (BgInput' patch_state Hydra)


canvasId :: String
canvasId = "hydra-canvas"


initialState :: forall patch_state. BgInput' patch_state Hydra -> State
initialState _ =
    { hydraReady : false, size : zero }


render :: forall patch_state m. State -> H.ComponentHTML (Action patch_state) () m
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


handleAction :: forall output patch_state m. MonadEffect m => Action patch_state -> H.HalogenM State (Action patch_state) () output m Unit
handleAction Initialize = do
    _ <- liftEffect $ Hydra.init canvasId
    H.modify_ $ _ { hydraReady = true }
handleAction (Receive { size }) =
    H.modify_ $ _ { size = size }


component :: forall query output patch_state m. MonadEffect m => H.Component query (BgInput' patch_state Hydra) output m
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