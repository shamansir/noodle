module MShape where

import Prelude

_in_sides = Fn.Input :: _ "sides"
_in_radius = Fn.Input :: _ "radius"
_in_smoothing = Fn.Input :: _ "smoothing"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( sides :: Value, radius :: Value, smoothing :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { sides : 60, radius : 0.3, smoothing : 0.01 }
        { out : ?out_default }
        $ fml.make $ do
            sides <- P.receive _in_sides
            radius <- P.receive _in_radius
            smoothing <- P.receive _in_smoothing
            -- Shape _in_sides _in_radius _in_smoothing
            P.send _out_out ?out_out
