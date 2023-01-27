module Blessed.UI.Forms.TextArea.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( mouse :: Boolean
    , keys :: Boolean
    , inputOnFocus :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type TextAreaAttribute r e = C.Attribute (Input.OptionsRow + OptionsRow + r) e


textAreaOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextAreaAttribute r e
textAreaOption = C.option


mouse :: forall r e. Boolean -> TextAreaAttribute ( mouse :: Boolean | r ) e
mouse = textAreaOption (Proxy :: _ "mouse")


keys :: forall r e. Boolean -> TextAreaAttribute ( keys :: Boolean | r ) e
keys = textAreaOption (Proxy :: _ "keys")


inputOnFocus :: forall r e. Boolean -> TextAreaAttribute ( inputOnFocus :: Boolean | r ) e
inputOnFocus = textAreaOption (Proxy :: _ "inputOnFocus")
