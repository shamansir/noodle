module Web.Components.ValueEditor.Numeric where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe)
import Data.Number as Number

import DOM.HTML.Indexed.InputType (InputType(..)) as I

import Noodle.Repr.HasFallback (class HasFallback)

import Web.Components.ValueEditor as VE


data Action repr
    = Skip
    | SendValue repr
    | CloseEditor
    | Receive (VE.Input repr)


type NumericMW repr = VE.MW_ Number repr



editorId = VE.EditorId "numeric" :: VE.EditorId


makeMW :: forall repr. (repr -> Maybe Number) -> (Number -> repr) -> NumericMW repr
makeMW fromRepr toRepr =
    { fromRepr
    , toRepr
    , decode : Number.fromString
    , encode : show
    }


editor :: forall repr m. HasFallback repr => MonadEffect m => (repr -> Maybe Number) -> (Number -> repr) -> VE.ValueEditor repr m
editor fromRepr toRepr =
    VE.editor I.InputNumber editorId $ makeMW fromRepr toRepr
