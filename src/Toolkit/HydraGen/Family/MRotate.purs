module MRotate where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_angle = Fn.Input :: _ "angle"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, angle :: Value, speed :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, angle : 10, speed : ?speed_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            angle <- P.receive _in_angle
            speed <- P.receive _in_speed
            -- Rotate _in_what _in_angle _in_speed
            P.send _out_out ?out_out
