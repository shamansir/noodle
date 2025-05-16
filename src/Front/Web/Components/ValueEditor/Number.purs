module Web.Components.ValueEditor.Number where

import Prelude


import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Attributes.Color.Extra as HCColorX
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP

import Web.Components.ValueEditor as VE

data Action = Increment | Decrement



editor :: forall repr m. H.Component VE.Query VE.Input (VE.Output repr) m
editor =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }
    where
    initialState _ = 0

    render state =
        HH.input
            [ HHP.type_ I.InputNumber
            , HHP.width 40, HHP.height 9
            , HHP.min 0.0
            , HHP.max 20.0
            , HHP.style $ "background-color: " <> HC.printColor (Just $ P.hColorOf inputBackgroundColor) <> "; "
                <> "color: " <> HC.printColor (Just $ P.hColorOf inputTextColor) <> "; "
                <> "border-radius: 5px; "
                <> "border: 1px solid " <> HC.printColor (Just $ P.hColorOf inputBorderColor) <> ";"
                <> HHP.position_ HHP.Abs { x : theInletPos.x + 7.0, y : theInletPos.y - 20.0 }
            -- , HP.step $ I.Step step
            -- , HP.value $ show val
            -- , HE.onValueInput (Number.fromString >>> maybe def handler)
            ]

    theInletPos = { x : 200.0, y : 200.0 } -- FIXME

    handleAction = case _ of
        Decrement ->
            H.modify_ \state -> state - 1

        Increment ->
            H.modify_ \state -> state + 1

    inputBorderColor = _.i600 $ Palette.yellow
    inputTextColor = _.i100 $ Palette.cyan
    inputBackgroundColor = _.i900 $ Palette.yellow
