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

import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box


type OptionsRow r =
    ( keys :: Boolean
    , vi :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type FormAttribute r e = C.Attribute (Box.OptionsRow + OptionsRow + r) e


formOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> FormAttribute r e
formOption = C.option


keys :: forall r e. Boolean -> FormAttribute ( keys :: Boolean | r ) e
keys = formOption (Proxy :: _ "keys")


vi :: forall r e. Boolean -> FormAttribute ( keys :: Boolean | r ) e
vi = formOption (Proxy :: _ "keys")