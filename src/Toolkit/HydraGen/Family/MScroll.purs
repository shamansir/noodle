module MScroll where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speedX = Fn.Input :: _ "speedX"
_in_speedY = Fn.Input :: _ "speedY"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, scrollX : 0.5, scrollY : 0.5, speedX : ?speedX_default, speedY : ?speedY_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            scrollY <- P.receive _in_scrollY
            speedX <- P.receive _in_speedX
            speedY <- P.receive _in_speedY
            -- Scroll _in_what _in_scrollX _in_scrollY _in_speedX _in_speedY
            P.send _out_out ?out_out
