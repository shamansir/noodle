module Toolkit.HydraGen.Family.Geometry.FRotate where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_what = Fn.Input :: _ "what"
_in_angle = Fn.Input :: _ "angle"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, angle :: Value, speed :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, angle : 10, speed : ?speed_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            angle <- P.receive _in_angle
            speed <- P.receive _in_speed
            -- Rotate what angle speed
            P.send _out_out ?out_out
