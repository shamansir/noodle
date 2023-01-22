module Blessed.UI.Checkbox.Option where

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


import Blessed.UI.Box.Event (Event)
import Blessed.UI.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( text :: String
    , checked :: Boolean
    , mouse :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type CheckboxAttribute r = C.Attribute (Input.OptionsRow + OptionsRow + r) Event


cbOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> CheckboxAttribute r
cbOption = C.option


text :: forall r. String -> CheckboxAttribute ( text :: String | r )
text = cbOption (Proxy :: _ "text")


checked :: forall r. Boolean -> CheckboxAttribute ( checked :: Boolean | r )
checked = cbOption (Proxy :: _ "checked")


mouse :: forall r. Boolean -> CheckboxAttribute ( mouse :: Boolean | r )
mouse = cbOption (Proxy :: _ "mouse")
