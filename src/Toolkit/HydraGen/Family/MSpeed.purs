module MSpeed where

import Prelude

_in_v = Fn.Input :: _ "v"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( v :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { v : 1 }
        { out : ?out_default }
        $ fml.make $ do
            v <- P.receive _in_v
            -- Speed _in_v
            P.send _out_out ?out_out
