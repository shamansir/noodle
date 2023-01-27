module Blessed.UI.DataDisplay.ProgressBar.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B
import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.FgBg (Evaluated) as FgBg

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( orientation :: Orientation
    , style_bar :: Array (FgBgOption ())
    , filled :: Int
    , pch :: Char
    , keys :: Boolean
    , mouse :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type ProgressBarAttribute r e = C.Attribute (Input.OptionsRow + OptionsRow + r) e


pbOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ProgressBarAttribute r e
pbOption = C.option


orientation :: forall r e. Orientation -> ProgressBarAttribute ( orientation :: Orientation | r ) e
orientation = pbOption (Proxy :: _ "orientation")


mouse :: forall r e. Boolean -> ProgressBarAttribute ( mouse :: Boolean | r ) e
mouse = pbOption (Proxy :: _ "mouse")


keys :: forall r e. Boolean -> ProgressBarAttribute ( keys :: Boolean | r ) e
keys = pbOption (Proxy :: _ "keys")


filled :: forall r e. Int -> ProgressBarAttribute ( filled :: Int | r ) e
filled = pbOption (Proxy :: _ "filled")


pch :: forall r e. Char -> ProgressBarAttribute ( pch :: Char | r ) e
pch = pbOption (Proxy :: _ "pch")


style_bar ∷ forall r e. Array (FgBgOption ()) -> ProgressBarAttribute ( style_bar :: Array (FgBgOption ()) | r ) e
style_bar = pbOption ( Proxy :: Proxy "style_bar" )