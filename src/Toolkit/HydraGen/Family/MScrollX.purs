module MScrollX where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, scrollX :: Value, speed :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, scrollX : 0.5, speed : ?speed_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            -- ScrollX _in_what _in_scrollX _in_speed
            P.send _out_out ?out_out
