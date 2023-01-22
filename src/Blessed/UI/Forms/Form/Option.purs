module Blessed.UI.Forms.Form.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box


type OptionsRow r =
    ( keys :: Boolean
    , vi :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type FormAttribute r = C.Attribute (Box.OptionsRow + OptionsRow + r) Event


formOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> FormAttribute r
formOption = C.option


keys :: forall r. Boolean -> FormAttribute ( keys :: Boolean | r )
keys = formOption (Proxy :: _ "keys")


vi :: forall r. Boolean -> FormAttribute ( keys :: Boolean | r )
vi = formOption (Proxy :: _ "keys")