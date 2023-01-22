module Blessed.UI.Line.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Element.Event (Event)
import Blessed.UI.List.Option (OptionsRow) as List


type OptionsRow r =
    ( cwd :: String
    | r
    )
type Options = Record (OptionsRow ())


type FileManagerAttribute r = C.Attribute (List.OptionsRow + OptionsRow + r) Event


fmOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> FileManagerAttribute r
fmOption = C.option


cwd :: forall r. Color -> FileManagerAttribute ( cwd :: String | r )
cwd = fmOption (Proxy :: _ "cwd")
