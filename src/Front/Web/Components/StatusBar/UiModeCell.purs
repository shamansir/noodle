
module Web.Components.StatusBar.UiModeCell where

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
import Halogen.Svg.Attributes.Transform (Transform(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Data.Text.Format (Tag) as T

import Web.Layer (TargetLayer(..))

import Front.Shared.StatusBarCells as Cells
import Web.Components.AppScreen.UiMode (UiModeKey(..))
import Web.Components.AppScreen.UiMode (modeString, nextKey) as UiMode


type Input =
    { currentMode :: UiModeKey
    , fontSize :: Number
    }


type State =
    { currentMode :: UiModeKey
    , fontSize :: Number
    }


data Action
    = Initialize
    | Receive Input
    | RequestNextUiMode


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
    initialState { currentMode, fontSize } =
        { currentMode, fontSize }

    render SVG state =
            HS.g
                []
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px state.fontSize
                    , HSA.dominant_baseline HSA.Central
                    , HSA.transform [ HSA.Translate 0.0 $ (state.fontSize / 2.0) + 7.0 ]
                    ]
                    $ pure $ HH.text $ UiMode.modeString state.currentMode
                {-
                , HS.circle
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.r 5.0
                    , HSA.cx $ zoomTextWidth + 5.0
                    , HSA.cy 0.0
                    , HE.onClick $ const RequestNextUiMode
                    ]
                -}
                ]

    render HTML _ =
        HH.span [] []

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive { currentMode, fontSize } ->
            H.put { currentMode, fontSize }
        RequestNextUiMode -> do
            { currentMode } <- H.get
            H.raise $ Cells.ChangeMode $ UiMode.nextKey currentMode
