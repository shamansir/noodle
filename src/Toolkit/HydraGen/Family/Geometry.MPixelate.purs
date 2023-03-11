module Geometry.MPixelate where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_what = Fn.Input :: _ "what"
_in_pixelX = Fn.Input :: _ "pixelX"
_in_pixelY = Fn.Input :: _ "pixelY"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, pixelX :: Value, pixelY :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, pixelX : 20, pixelY : 20 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            pixelX <- P.receive _in_pixelX
            pixelY <- P.receive _in_pixelY
            -- Pixelate what pixelX pixelY
            P.send _out_out ?out_out
