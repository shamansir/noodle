module Blessed.UI.Boxes.Line.Option where

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
    ( orientation :: Orientation
    , type :: B.BorderType
    , bg :: Color
    , fg :: Color
    , ch :: Char
    | r
    )
type Options = Record (OptionsRow ())


type LineAttribute r e = C.Attribute (Box.OptionsRow + OptionsRow + r) e


lineOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> LineAttribute r e
lineOption = C.option


fg :: forall r e. Color -> LineAttribute ( fg :: Color | r ) e
fg = lineOption (Proxy :: _ "fg")


bg :: forall r e. Color -> LineAttribute ( bg :: Color | r ) e
bg = lineOption (Proxy :: _ "bg")


ch :: forall r e. Char -> LineAttribute ( ch :: Char | r ) e
ch = lineOption (Proxy :: _ "ch")


orientation :: forall r e. Orientation -> LineAttribute ( orientation :: Orientation | r ) e
orientation = lineOption (Proxy :: _ "orientation")
