module MScrollY where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, scrollY :: Value, speed :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, scrollY : 0.5, speed : ?speed_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            scrollY <- P.receive _in_scrollY
            speed <- P.receive _in_speed
            -- ScrollY _in_what _in_scrollY _in_speed
            P.send _out_out ?out_out
