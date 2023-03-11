module Toolkit.HydraGen.Family.Source.FVoronoi where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_scale = Fn.Input :: _ "scale"
_in_speed = Fn.Input :: _ "speed"
_in_blending = Fn.Input :: _ "blending"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( scale :: Value, speed :: Value, blending :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { scale : 5, speed : 0.3, blending : 0.3 }
        { out : ?out_default }
        $ Fn.make $ do
            scale <- P.receive _in_scale
            speed <- P.receive _in_speed
            blending <- P.receive _in_blending
            -- Voronoi scale speed blending
            P.send _out_out ?out_out
