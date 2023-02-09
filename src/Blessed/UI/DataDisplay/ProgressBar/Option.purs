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
import Blessed.Core.EndStyle (EndStyleOption)
import Blessed.Core.EndStyle (Evaluated) as EndStyle

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, ProgressBar)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( orientation :: Orientation
    , style_bar :: Array (EndStyleOption ())
    , filled :: Int
    , pch :: Char
    , keys :: Boolean
    , mouse :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type ProgressBarAttribute subj id r state e = C.Attribute subj id (Input.OptionsRow + OptionsRow + r) state e


pbOption
    :: forall subj id a r r' sym state e
     . Respresents ProgressBar subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ProgressBarAttribute subj id r state e
pbOption = C.option


orientation
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Orientation -> ProgressBarAttribute subj id ( orientation :: Orientation | r ) state e
orientation = pbOption (Proxy :: _ "orientation")


mouse
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Boolean -> ProgressBarAttribute subj id ( mouse :: Boolean | r ) state e
mouse = pbOption (Proxy :: _ "mouse")


keys
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Boolean -> ProgressBarAttribute subj id ( keys :: Boolean | r ) state e
keys = pbOption (Proxy :: _ "keys")


filled
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Int -> ProgressBarAttribute subj id ( filled :: Int | r ) state e
filled = pbOption (Proxy :: _ "filled")


pch
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Char -> ProgressBarAttribute subj id ( pch :: Char | r ) state e
pch = pbOption (Proxy :: _ "pch")


style_bar
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ProgressBar subj id
    => Array (EndStyleOption ()) -> ProgressBarAttribute subj id ( style_bar :: Array (EndStyleOption ()) | r ) state e
style_bar = pbOption ( Proxy :: Proxy "style_bar" )