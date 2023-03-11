module Geometry.MScrollX where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, scrollX :: Value, speed :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, scrollX : 0.5, speed : ?speed_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            -- ScrollX what scrollX speed
            P.send _out_out ?out_out
