module Blessed.UI.Text.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Align (HAlign)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Element.Event (Event)
import Blessed.UI.Element.Option (OptionsRow) as Element


type OptionsRow r =
    ( fill :: Color
    , align :: HAlign
    | r
    )
type Options = Record (OptionsRow ())


type TextAttribute r = C.Attribute (Element.OptionsRow + OptionsRow + r) Event


textOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextAttribute r
textOption = C.option


fill :: forall r. Color -> TextAttribute ( fill :: Color | r)
fill = textOption (Proxy :: _ "fill")


align :: forall r. HAlign -> TextAttribute ( align :: HAlign | r)
align = textOption (Proxy :: _ "align")
