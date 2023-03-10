module MModulatePixelate where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_multiple = Fn.Input :: _ "multiple"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, multiple :: Value, offset :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, multiple : 10, offset : 3 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            multiple <- P.receive _in_multiple
            offset <- P.receive _in_offset
            -- ModulatePixelate _in_what _in_with _in_multiple _in_offset
            P.send _out_out ?out_out
