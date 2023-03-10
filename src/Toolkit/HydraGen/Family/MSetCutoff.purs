module MSetCutoff where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_cutoff = Fn.Input :: _ "cutoff"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, cutoff :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, cutoff : 2 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            cutoff <- P.receive _in_cutoff
            -- SetCutoff _in_a _in_cutoff
            P.send _out_out ?out_out
