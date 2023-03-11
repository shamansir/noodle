module Toolkit.HydraGen.Family.Source.FGradient where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( speed :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { speed : ?speed_default }
        { out : ?out_default }
        $ Fn.make $ do
            speed <- P.receive _in_speed
            -- Gradient speed
            P.send _out_out ?out_out
