module MUpdate where

import Prelude

_in_fn = Fn.Input :: _ "fn"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( fn :: UpdateFn )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { fn : ?fn_default }
        { out : ?out_default }
        $ fml.make $ do
            fn <- P.receive _in_fn
            -- Update _in_fn
            P.send _out_out ?out_out
