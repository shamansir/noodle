module Blessed.UI.Textarea.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Box.Event (Event)
import Blessed.UI.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( mouse :: Boolean
    , keys :: Boolean
    , inputOnFocus :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type TextAreaAttribute r = C.Attribute (Input.OptionsRow + OptionsRow + r) Event


textAreaOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextAreaAttribute r
textAreaOption = C.option


mouse :: forall r. Boolean -> TextAreaAttribute ( mouse :: Boolean | r )
mouse = textAreaOption (Proxy :: _ "mouse")


keys :: forall r. Boolean -> TextAreaAttribute ( keys :: Boolean | r )
keys = textAreaOption (Proxy :: _ "keys")


inputOnFocus :: forall r. Boolean -> TextAreaAttribute ( inputOnFocus :: Boolean | r )
inputOnFocus = textAreaOption (Proxy :: _ "inputOnFocus")
