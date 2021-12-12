module Hydra.Component.Patch where


import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

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
import App.Toolkit.UI (TellPatch(..), InformApp(..)) as UI
import Hydra.Toolkit.UI.Action as ToolkitUI
import Hydra.Queue (Queue)
import Hydra.Queue as Queue


type State =
    { hydraReady :: Boolean
    , size :: Size
    , queue :: Queue
    }


data Action
    = Initialize
    | Receive UI.PatchInput


canvasId :: String
canvasId = "hydra-canvas"


initialState :: UI.PatchInput -> State
initialState { patchState } =
    { hydraReady : false, size : zero, queue : patchState }


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
handleAction (Receive { size, patchState }) =
    H.modify_ $ _ { size = size, queue = patchState }


handleQuery :: forall action m a. UI.PatchQuery a -> H.HalogenM State action () UI.PatchOutput m (Maybe a)
handleQuery (UI.TellPatch (ToolkitUI.Store buffer texture) a) = do
    { queue } <-
        H.modify \state ->
                    state { queue = state.queue # Queue.toBuffer buffer texture }
    H.raise $ UI.Next queue
    pure $ Just a


component :: forall m. MonadEffect m => UI.PatchComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }
