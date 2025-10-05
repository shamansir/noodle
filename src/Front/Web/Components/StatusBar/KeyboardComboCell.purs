module Web.Components.StatusBar.KeyboardComboCell where

import Prelude


import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Attributes.Transform (Transform(..)) as HSA

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Web.Layer (TargetLayer(..))

import Front.Shared.StatusBarCells as Cells
import Web.Components.AppScreen.KeyboardLogic as KL


type Input = KL.Focus


type State = KL.Focus


data Action
    = Initialize
    | Receive Input


component :: forall query m. TargetLayer -> H.Component query Input Cells.Output m
component targetLayer =
    H.mkComponent
        { initialState : initialState
        , render : render targetLayer
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }

    where
    initialState focus =
        focus

    render SVG state =
        HS.g []
            [ HS.text
                [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                        , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 10.0
                        , HSA.dominant_baseline HSA.Central
                        , HSA.transform [ HSA.Translate 0.0 11.0 ]
                        ]
                [ HH.text $ fullText state ]
            ]

    render HTML state =
        HH.span [] []
        -- HH.span [ {- HHP.style "postion: absolute;" -} ] [ HH.text "HTML:TEST" ]

    fullText = KL.toSequence >>> String.joinWith "-"

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive focus ->
            H.put focus
