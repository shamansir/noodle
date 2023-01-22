module Blessed.UI.Line.Option where

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


import Blessed.UI.Element.Event (Event)
import Blessed.UI.Element.Option (OptionsRow) as Box


type OptionsRow r =
    ( orientation :: Orientation
    , type :: B.BorderType
    , bg :: Color
    , fg :: Color
    , ch :: Char
    | r
    )
type Options = Record (OptionsRow ())


type LineAttribute r = C.Attribute (Box.OptionsRow + OptionsRow + r) Event


lineOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> LineAttribute r
lineOption = C.option


fg :: forall r. Color -> LineAttribute ( fg :: Color | r )
fg = lineOption (Proxy :: _ "fg")


bg :: forall r. Color -> LineAttribute ( bg :: Color | r )
bg = lineOption (Proxy :: _ "bg")


ch :: forall r. Char -> LineAttribute ( ch :: Char | r )
ch = lineOption (Proxy :: _ "ch")


orientation :: forall r. Orientation -> LineAttribute ( orientation :: Orientation | r )
orientation = lineOption (Proxy :: _ "orientation")
