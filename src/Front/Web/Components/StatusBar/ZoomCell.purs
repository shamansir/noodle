module Web.Components.StatusBar.ZoomCell where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.String (take) as String
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Data.Text.Format (Tag) as T

import Web.Layer (TargetLayer(..))

import Front.Shared.StatusBarCells as Cells


type Input =
    { currentZoom :: Number
    , fontSize :: Number
    }


type State =
    { currentZoom :: Number
    , fontSize :: Number
    }


data Action
    = Initialize
    | Receive Input
    | RequestZoomReset


component :: forall query m. TargetLayer -> H.Component query Input Cells.Output m
component targetLayer =
    H.mkComponent
        { initialState
        , render : render targetLayer
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }

    where
    initialState { currentZoom, fontSize } =
        { currentZoom, fontSize }

    zoomTextWidth = 35.0

    render SVG state =
        if state.currentZoom /= 1.0 then
            HS.g
                []
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px state.fontSize
                    , HSA.dominant_baseline HSA.Central
                    ]
                    [ HH.text $ String.take 5 $ show state.currentZoom ]
                , HS.circle
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.r 5.0
                    , HSA.cx $ zoomTextWidth + 5.0
                    , HSA.cy 0.0
                    , HE.onClick $ const RequestZoomReset
                    ]
                ]
        else HH.text ""

    render HTML _ =
        HH.span [] []

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive { currentZoom, fontSize } ->
            H.put { currentZoom, fontSize }
        RequestZoomReset ->
            H.raise Cells.ResetZoom