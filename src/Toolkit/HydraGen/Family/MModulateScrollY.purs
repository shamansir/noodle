module MModulateScrollY where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, scrollY :: Value, speed :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, scrollY : 0.5, speed : ?speed_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            scrollY <- P.receive _in_scrollY
            speed <- P.receive _in_speed
            -- ModulateScrollY _in_what _in_with _in_scrollY _in_speed
            P.send _out_out ?out_out
