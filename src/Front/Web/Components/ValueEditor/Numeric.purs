module Web.Components.ValueEditor.Numeric where

import Prelude

import Debug as Debug


import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I

import Noodle.Repr.HasFallback (fallback, class HasFallback)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Attributes.Color.Extra as HCColorX
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP

import Web.Components.ValueEditor as VE


data Action repr
    = Skip
    | SendValue repr


editor :: forall repr m. HasFallback repr => Monad m => (repr -> Maybe Number) -> (Number -> repr) -> VE.ValueEditor repr Unit m
editor fromRepr toRepr sendValue =
    H.mkComponent
        { initialState
        , render
        , eval : H.mkEval H.defaultEval { handleAction = handleAction }
        }
    where
    initialState { pos, currentValue } = { pos, currentValue }

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
                <> HHP.position_ HHP.Abs { x : state.pos.x + 7.0, y : state.pos.y - 20.0 }
            -- , HP.step $ I.Step step
            , HHP.value $ maybe "-" show $ fromRepr state.currentValue
            , HE.onValueInput (Number.fromString >>> map toRepr >>> Debug.spy "repr" >>> maybe Skip SendValue)
            ]

    handleAction = case _ of
        Skip ->
            pure unit

        SendValue val ->
            H.lift $ sendValue val

    inputBorderColor = _.i600 $ Palette.yellow
    inputTextColor = _.i100 $ Palette.cyan
    inputBackgroundColor = _.i900 $ Palette.yellow
