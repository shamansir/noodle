module MPixelate where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_pixelX = Fn.Input :: _ "pixelX"
_in_pixelY = Fn.Input :: _ "pixelY"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, pixelX :: Value, pixelY :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, pixelX : 20, pixelY : 20 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            pixelX <- P.receive _in_pixelX
            pixelY <- P.receive _in_pixelY
            -- Pixelate _in_what _in_pixelX _in_pixelY
            P.send _out_out ?out_out
