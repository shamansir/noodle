module Hydra.Component.Node.Color where


import Prelude

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

import App.Toolkit.UI as UI

import Noodle.Node as Node

import Hydra (Hydra, Value(..))
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..))


type State = Array Color


data Action
    = Todo


bodyWidth = 110.0 -- FIXME: pass from outside


initialState :: UI.NodeInput Hydra -> State
initialState { node } =
    {- Node.defaultOfInlet "seq" node
        <#> HydraE.seq -
         #  fromMaybe [] -}
    [ RGB 255 255 255, RGB 255 0 0, RGB 0 255 0, RGB 0 0 255 ]


render :: forall m. State -> H.ComponentHTML Action () m
render colors =
    HS.g
        []
        [ (HS.g [] $ Array.mapWithIndex colorRect colors)
        ]
    where
        colorsCount = Array.length colors
        colorRectWidth = bodyWidth / toNumber colorsCount
        colorRect i color =
            HS.g
                [ ]
                [ HS.rect
                    [ HSA.x $ colorRectWidth * toNumber i
                    , HSA.y 0.0
                    , HSA.width colorRectWidth
                    , HSA.height 55.0
                    , HSA.fill $ Just color
                    ]
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = case _ of
    Todo ->
        pure unit


component :: forall m. UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }