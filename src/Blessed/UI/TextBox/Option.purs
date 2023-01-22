module Blessed.UI.TextBox.Option where

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
    ( secret :: String
    , censor :: String
    | r
    )
type Options = Record (OptionsRow ())


type TextBoxAttribute r = C.Attribute (Input.OptionsRow + OptionsRow + r) Event


textBoxOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TextBoxAttribute r
textBoxOption = C.option


secret :: forall r. String -> TextBoxAttribute ( secret :: String | r )
secret = textBoxOption (Proxy :: _ "secret")


censor :: forall r. String -> TextBoxAttribute ( censor :: String | r )
censor = textBoxOption (Proxy :: _ "censor")
