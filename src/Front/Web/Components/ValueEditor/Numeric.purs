module Web.Components.ValueEditor.Numeric where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE

import Halogen.Svg.Attributes.Color as HC
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP

import DOM.HTML.Indexed.InputType (InputType(..)) as I
import Web.UIEvent.KeyboardEvent as KE

import Noodle.Repr.HasFallback (fallback, class HasFallback)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

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


editor :: forall repr m. HasFallback repr => Monad m => (repr -> Maybe Number) -> (Number -> repr) -> VE.ValueEditor repr m
editor fromRepr toRepr =
    VE.editor I.InputNumber editorId $ makeMW fromRepr toRepr
