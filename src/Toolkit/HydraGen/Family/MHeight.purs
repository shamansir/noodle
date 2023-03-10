module MHeight where

import Prelude



_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { }
        { out : ?out_default }
        $ fml.make $ do
            -- Height
            P.send _out_out ?out_out
