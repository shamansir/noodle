module Hydra.Component.Node.Pi where


import Prelude (const, ($), Unit, unit, (/))

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (Maybe(..))
import Color as C
import Color.Extra (toSvg) as C

import App.Toolkit.UI (FromNode(..)) as UI

import Hydra as Hydra
import Hydra.Toolkit.UI.Components (NodeComponent, NodeOutput) as UI

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize as HSA
import Halogen.Svg.Attributes.CSSLength as HSA
import Halogen.Svg.Attributes.TextAnchor as HSA
import Halogen.Svg.Attributes.Baseline as HSA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH


data Action
    = Send


bodyWidth = 110.0 -- FIXME: pass from outside
bodyHeight = 80.0 -- FIXME: pass from outside


render :: forall m. Unit -> H.ComponentHTML Action () m
render _ =
    HS.g
        [ ]
        [ HS.text
            [ HE.onClick $ const $ Send
            --, HSA.stroke $ Just $ C.toSvg $ C.rgba 255 255 255 0.7
            , HSA.fill $ Just $ C.toSvg $ C.rgba 255 255 255 0.7
            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 60.0
            , HSA.x $ bodyWidth / 2.0
            , HSA.y $ bodyHeight / 2.0
            , HSA.text_anchor $ HSA.AnchorMiddle
            , HSA.dominant_baseline $ HSA.BaselineMiddle
            --, HSA.alignment_baseline $ HSA.BaselineMiddle
            ]
            [ HH.text "π" ]
        ]


handleAction :: forall m. Action -> H.HalogenM Unit Action () UI.NodeOutput m Unit
handleAction = case _ of
    Send ->
        H.raise $ UI.SendToOutlet "pi" $ Hydra.pi


component :: forall m. UI.NodeComponent m
component =
    H.mkComponent
        { initialState : const unit
        , render
        , eval: H.mkEval H.defaultEval
            { initialize = Just Send
            , handleAction = handleAction
            }
        }