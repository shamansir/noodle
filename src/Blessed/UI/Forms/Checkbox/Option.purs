module Blessed.UI.Forms.Checkbox.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.FgBg (Evaluated) as FgBg

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( text :: String
    , checked :: Boolean
    , mouse :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type CheckboxAttribute r e = C.Attribute (Input.OptionsRow + OptionsRow + r) e


cbOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> CheckboxAttribute r e
cbOption = C.option


text :: forall r e. String -> CheckboxAttribute ( text :: String | r ) e
text = cbOption (Proxy :: _ "text")


checked :: forall r e. Boolean -> CheckboxAttribute ( checked :: Boolean | r ) e
checked = cbOption (Proxy :: _ "checked")


mouse :: forall r e. Boolean -> CheckboxAttribute ( mouse :: Boolean | r ) e
mouse = cbOption (Proxy :: _ "mouse")
