module Blessed.UI.Boxes.Text.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Align (HAlign)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Base.Element.Option (OptionsRow) as Element


type OptionsRow r =
    ( fill :: Color
    , align :: HAlign
    | r
    )
type Options = Record (OptionsRow ())


type TextAttribute r e = C.Attribute (Element.OptionsRow + OptionsRow + r) e


textOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextAttribute r e
textOption = C.option


fill :: forall r e. Color -> TextAttribute ( fill :: Color | r ) e
fill = textOption (Proxy :: _ "fill")


align :: forall r e. HAlign -> TextAttribute ( align :: HAlign | r ) e
align = textOption (Proxy :: _ "align")
