module Web.Components.CommandInput where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Number as Number
import Data.Int as Int
import Data.String as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Noodle.Id (FamilyR) as Id
import Noodle.Toolkit (Toolkit, class HoldsFamilies)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (fallback, class HasFallback)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr)

import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Attributes.Color.Extra as HCColorX
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP


import Noodle.Text.NdfFile.Command.FromInput as FI


type Input =
    { pos ::
        { x :: Number, y :: Number }
    }


type State =
    { pos ::
        { x :: Number, y :: Number }
    }


data Action
    = Skip
    | TryExecuteCommand String
    | RaiseCloseInput
    | Receive Input


data Output sr cr m
    = ExecuteCommand (FI.CommandResult sr cr m)
    | CloseCommandInput



component
    :: forall tk fs sr cr query m
     . MonadEffect m
    => HasFallback sr
    => HasFallback cr
    => ParseableRepr cr
    => ValueTagged cr
    => HoldsFamilies sr cr m fs
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit tk fs sr cr m
    -> H.Component query Input (Output sr cr m) m
component toolkit =
    H.mkComponent
        { initialState
        , render
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }
    where
    initialState { pos } = { pos }
    fullWidth = 200.0

    render state =
        HH.input
            [ HHP.type_ I.InputText
            , HHP.width (fromMaybe 0 $ Int.fromNumber fullWidth), HHP.height 9
            , HHP.style $ "background-color: " <> HC.printColor (Just $ P.hColorOf inputBackgroundColor) <> "; "
                <> "color: " <> HC.printColor (Just $ P.hColorOf inputTextColor) <> "; "
                <> "border-radius: 5px; "
                <> "border: 1px solid " <> HC.printColor (Just $ P.hColorOf inputBorderColor) <> ";"
                <> HHP.position_ HHP.Abs { x : state.pos.x - (fullWidth / 2.0), y : state.pos.y }
            -- , HHP.value $ maybe "-" show $ fromRepr state.currentValue
            , HE.onValueInput TryExecuteCommand
            -- , HE.onValueChange (Debug.spy "onValueChange" >>> const Skip)
            -- , HE.onKeyUp (\kevt ->
            --         if (String.toLower (KE.key kevt) == "escape") || (String.toLower (KE.key kevt) == "enter")
            --             then Debug.spy "close editor" CloseEditor
            --             else Skip
            --     )
            ]

    handleAction = case _ of
        Skip ->
            pure unit

        TryExecuteCommand commandStr -> do
            cmdResult <- FI.tryExecute toolkit commandStr
            H.raise $ ExecuteCommand cmdResult

        RaiseCloseInput ->
            H.raise CloseCommandInput

        Receive { pos } ->
            H.modify_ _ { pos = pos }

    inputBorderColor = _.i600 $ Palette.yellow
    inputTextColor = _.i100 $ Palette.cyan
    inputBackgroundColor = _.i900 $ Palette.yellow
