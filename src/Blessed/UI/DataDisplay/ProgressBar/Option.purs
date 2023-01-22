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


type ProgressBarAttribute r = C.Attribute (Input.OptionsRow + OptionsRow + r) Event


pbOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ProgressBarAttribute r
pbOption = C.option


orientation :: forall r. Orientation -> ProgressBarAttribute ( orientation :: Orientation | r )
orientation = pbOption (Proxy :: _ "orientation")


mouse :: forall r. Boolean -> ProgressBarAttribute ( mouse :: Boolean | r )
mouse = pbOption (Proxy :: _ "mouse")


keys :: forall r. Boolean -> ProgressBarAttribute ( keys :: Boolean | r )
keys = pbOption (Proxy :: _ "keys")


filled :: forall r. Int -> ProgressBarAttribute ( filled :: Int | r )
filled = pbOption (Proxy :: _ "filled")


pch :: forall r. Char -> ProgressBarAttribute ( pch :: Char | r )
pch = pbOption (Proxy :: _ "pch")


style_bar ∷ forall r. Array (FgBgOption ()) -> ProgressBarAttribute ( style_bar :: Array (FgBgOption ()) | r )
style_bar = pbOption ( Proxy :: Proxy "style_bar" )