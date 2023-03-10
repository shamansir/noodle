module MSetSmooth where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, smooth :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, smooth : 0.4 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            smooth <- P.receive _in_smooth
            -- SetSmooth _in_a _in_smooth
            P.send _out_out ?out_out
