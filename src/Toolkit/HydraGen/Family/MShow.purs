module MShow where

import Prelude

_in_a = Fn.Input :: _ "a"


_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, ?ch_type )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, ?ch_default }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            --
            -- Show _in_a ?input
            P.send _out_out ?out_out
