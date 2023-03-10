module MNoise where

import Prelude

_in_scale = Fn.Input :: _ "scale"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( scale :: Value, offset :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { scale : 10, offset : 0.1 }
        { out : ?out_default }
        $ fml.make $ do
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            -- Noise _in_scale _in_offset
            P.send _out_out ?out_out
