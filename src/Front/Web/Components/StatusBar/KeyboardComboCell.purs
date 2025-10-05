module Web.Components.StatusBar.KeyboardComboCell where

import Prelude


import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS

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
        HS.g [] [ HS.text [] [ HH.text $ fullText state ] ]

    render HTML state =
        HH.span [] []
        -- HH.span [ {- HHP.style "postion: absolute;" -} ] [ HH.text "HTML:TEST" ]

    fullText = KL.toSequence >>> String.joinWith "-"

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive focus ->
            H.put focus
