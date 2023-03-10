module MR where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_scale = Fn.Input :: _ "scale"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, scale :: Value, offset :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, scale : 1, offset : ?offset_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            -- R _in_what _in_scale _in_offset
            P.send _out_out ?out_out
