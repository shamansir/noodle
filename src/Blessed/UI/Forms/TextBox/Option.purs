module Blessed.UI.Forms.TextBox.Option where

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
    ( secret :: String
    , censor :: String
    | r
    )
type Options = Record (OptionsRow ())


type TextBoxAttribute r e = C.Attribute (Input.OptionsRow + OptionsRow + r) e


textBoxOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextBoxAttribute r e
textBoxOption = C.option


secret :: forall r e. String -> TextBoxAttribute ( secret :: String | r ) e
secret = textBoxOption (Proxy :: _ "secret")


censor :: forall r e. String -> TextBoxAttribute ( censor :: String | r ) e
censor = textBoxOption (Proxy :: _ "censor")
