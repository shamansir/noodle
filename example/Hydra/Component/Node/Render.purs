module Hydra.Component.Node.Render where


import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Color as C
import Math (floor)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((!!))
import Data.Typelevel.Num.Reps (d0, d1, d2)

import App.Toolkit.UI (FromNode(..)) as UI
import App.Emitters as E

import JetBrains.Palettes as P

import Noodle.Node (Node)
import Noodle.Node as Node

import Hydra (Hydra, Buffer(..), Texture(..), Value(..))
import Hydra as Hydra
import Hydra.Queue (Queue)
import Hydra.Queue as Queue
import Hydra.Extract as HydraE
import Hydra.Compile as Hydra
import Hydra.Compile (friendly, compact) as Compiler
import Hydra.Engine as HydraE
import Hydra.Component.Input as Input
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI
import Hydra.Toolkit.UI.Action as Queue

import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA

import Color.Extra (toSvg) as C


type State =
    { queue :: Queue
    , node :: Node Hydra
    }


data Action
    = NoOp
    | Receive Queue
    | Render


bodyWidth = 110.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: UI.NodeInput -> State
initialState { patchState, node } =
    { queue : patchState, node }


render :: forall m. State -> H.ComponentHTML Action () m
render { queue } =
    HS.g
        [ HSA.class_ $ H.ClassName "render-button"
        ]
        [ HS.circle
            [ HSA.cx 12.0
            , HSA.cy 12.0
            , HSA.stroke $ Just $ C.toSvg
                    $ if not $ Queue.isEmpty queue then C.rgba 0 255 0 1.0 else C.rgba 100 100 100 1.0
            , HSA.fill $ Just $ C.toSvg
                    $ if not $ Queue.isEmpty queue then C.rgba 0 100 0 1.0 else C.rgba 50 50 50 1.0
            , HSA.strokeWidth 2.0
            , HSA.r 10.0
            ]
        , HS.rect
            [ HSA.x 2.0
            , HSA.y 2.0
            , HSA.width 20.0
            , HSA.height 20.0
            , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
            , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
            , HE.onClick $ const $ Render
            -- , HP.style "cursor: pointer"
            ]
        ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    NoOp -> pure unit
    Receive queue -> do
        H.modify_ (_ { queue = queue })
    Render -> do
        { queue } <- H.get
        liftEffect $ do
            Console.log $ Hydra.compileWithRender Compiler.friendly queue
            HydraE.evaluate $ Hydra.compileWithRender Compiler.compact queue
        pure unit


component :: forall m. MonadEffect m => UI.NodeComponent m
component =
    H.mkComponent
        { initialState : initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive <<< _.patchState
            }
        }