module MSetResolution where

import Prelude

_in_width = Fn.Input :: _ "width"
_in_height = Fn.Input :: _ "height"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( width :: Value, height :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { width : ?width_default, height : ?height_default }
        { out : ?out_default }
        $ fml.make $ do
            width <- P.receive _in_width
            height <- P.receive _in_height
            -- SetResolution _in_width _in_height
            P.send _out_out ?out_out
