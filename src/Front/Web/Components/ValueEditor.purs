module Web.Components.ValueEditor where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as M
import Data.String as String

import Web.HTML (HTMLElement)
import Web.UIEvent.KeyboardEvent as KE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes.Color as HC
import Halogen.HTML.Properties.Extra (Position(..), position_) as HHP

import Front.Shared.Bounds (PositionXY)

import Noodle.Id as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


newtype EditorId = EditorId String

derive instance Eq EditorId
derive instance Ord EditorId

foreign import focus :: HTMLElement -> Effect Unit


type Def repr =
    { inlet :: Id.InletR
    , pos :: PositionXY
    , editor :: EditorId
    , currentValue :: ValueInChannel repr
    }


type Input v =
    { pos :: PositionXY
    , currentValue :: v
    }


data Output v
    = SendValue v
    | CloseEditor


type ValueEditor v m
    = ValueEditorComp v m


data Action repr
    = ASkip
    | ASendValue repr
    | ACloseEditor
    | AReceive (Input repr)


data Query a
    = Query a


type ValueEditorComp v m =
    H.Component Query (Input v) (Output v) m


type MW_ a repr = -- TODO: use `Repr` typeclasses
    { fromRepr :: repr -> Maybe a
    , toRepr :: a -> repr
    , decode :: String -> Maybe a
    , encode :: a -> String
    }


editor :: forall a repr m. HasFallback repr => Monad m => HHP.InputType -> MW_ a repr -> ValueEditor repr m
editor inputType mw =
    H.mkComponent
        { initialState
        , render
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< AReceive
            }
        }
    where
    initialState { pos, currentValue } = { pos, currentValue }

    render state =
        HH.input
        [ HHP.type_ inputType
            , HHP.width 40, HHP.height 9
            -- , HHP.min 0.0
            -- , HHP.max 20.0
            , HHP.style $ "background-color: " <> HC.printColor (Just $ P.hColorOf inputBackgroundColor) <> "; "
                <> "color: " <> HC.printColor (Just $ P.hColorOf inputTextColor) <> "; "
                <> "border-radius: 5px; "
                <> "border: 1px solid " <> HC.printColor (Just $ P.hColorOf inputBorderColor) <> ";"
                <> HHP.position_ HHP.Abs { x : state.pos.x + 7.0, y : state.pos.y - 20.0 }
            -- , HP.step $ I.Step step
            , HHP.value $ M.maybe "-" mw.encode $ mw.fromRepr state.currentValue
            , HE.onValueInput (mw.decode >>> map mw.toRepr >>> {- Debug.spy "repr" >>> -} M.maybe ASkip ASendValue)
            -- , HE.onValueChange (Debug.spy "onValueChange" >>> const Skip)
            , HE.onKeyUp (\kevt ->
                    if (String.toLower (KE.key kevt) == "escape") || (String.toLower (KE.key kevt) == "enter")
                        then ACloseEditor -- Debug.spy "close editor" CloseEditor
                        else ASkip
                )
            ]

    handleAction = case _ of
        ASkip ->
            pure unit

        ASendValue val -> do
            H.raise $ SendValue val

        ACloseEditor ->
            H.raise $ CloseEditor

        AReceive { pos, currentValue } ->
            H.modify_ _ { pos = pos, currentValue = currentValue }

    inputBorderColor = _.i600 $ Palette.yellow
    inputTextColor = _.i100 $ Palette.cyan
    inputBackgroundColor = _.i900 $ Palette.yellow

    {- TODO:
    panelContentRef = H.RefLabel $ "panel-content" <> reflectSymbol pid

      -- TODO: replace with `whenJust`
      H.getHTMLElementRef state.elRef >>= case _ of
        Nothing -> pure unit
        Just el -> do
          liftEffect $ setHTML el state.html

      [ HP.ref state.elRef ]
    -}

{-
    { create :: BlessedOp state m
    , inject :: v -> BlessedOp state m
    , transpose :: IntPositionXY -> BlessedOp state m
    }


imap :: forall a b state m. (a -> b) -> (b -> a) -> ValueEditor a state m -> ValueEditor b state m
imap aToB bToA editorF =
    \curValue sendValue ->
        editorF (bToA curValue) (aToB >>> sendValue)
            # \editor -> editor { inject = bToA >>> editor.inject }


toMaybe :: forall v state m. HasFallback v => ValueEditor v state m -> ValueEditor (Maybe v) state m
toMaybe = imap pure $ M.fromMaybe fallback


fromMaybe :: forall v state m. HasFallback v => ValueEditor (Maybe v) state m -> ValueEditor v state m
fromMaybe = imap (M.fromMaybe fallback) pure
-}
