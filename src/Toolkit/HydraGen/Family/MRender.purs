module MRender where

import Prelude

_in_from = Fn.Input :: _ "from"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( from :: From )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { from : All }
        { out : ?out_default }
        $ fml.make $ do
            from <- P.receive _in_from
            -- Render _in_from
            P.send _out_out ?out_out
