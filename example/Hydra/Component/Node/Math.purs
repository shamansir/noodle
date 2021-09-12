module Hydra.Component.Node.Math where


import Prelude

import Color (rgb, rgba) as C

-- import Data.String.Read (read)
--import Data.Parse
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

import App.Toolkit.UI (FromNode(..)) as UI

import Hydra (Operation(..))
import Hydra as Hydra
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Color.Extra (toSvg) as C


type State = Hydra.Operation


data Action
    = Select Hydra.Operation


operations :: Array Operation
operations = [ Addition, Division, Multiplication, Subtraction ]



bodyWidth = 110.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: UI.NodeInput -> State
initialState _ =
    Addition


render :: forall m. State -> H.ComponentHTML Action () m
render currentOp =
    HS.g
        [ HSA.class_ $ H.ClassName "math-node" ]
        [ HS.g [] $ Array.mapWithIndex operatorButton operations
        ]
    where
        operatorButton idx op =
            HS.g
                [ {-HSA.class_ $ H.ClassName "buffer-button"
                , -} HSA.class_ $ H.ClassName $ "operator-button"
                ]
                [ HS.text
                    [ HSA.fill $ Just $ C.toSvg
                        $ if op == currentOp
                            then C.rgb 255 255 255
                            else C.rgb 100 100 100
                    , HSA.x $ toNumber idx * 10.0
                    , HSA.y 0.0
                    ]
                    [ HH.text $ show op ]
                , HS.rect
                    [ HSA.x $ toNumber idx * 10.0
                    , HSA.y 0.0
                    , HSA.width 10.0
                    , HSA.height 10.0
                    , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HE.onClick $ const $ Select op
                    -- , HP.style "cursor: pointer"
                    ]
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    Select operation -> do
        H.put operation
        H.raise $ UI.SendToInlet "op" $ Hydra.fromOp operation


component :: forall m. UI.NodeComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }