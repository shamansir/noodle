module Blessed.UI.Lists.FileManager.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( cwd :: String
    | r
    )
type Options = Record (OptionsRow ())


type FileManagerAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


fmOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> FileManagerAttribute r e
fmOption = C.option


cwd :: forall r e. Color -> FileManagerAttribute ( cwd :: String | r ) e
cwd = fmOption (Proxy :: _ "cwd")
