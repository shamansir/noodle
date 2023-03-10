module MSetScale where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_scale = Fn.Input :: _ "scale"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, scale :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, scale : 10 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            scale <- P.receive _in_scale
            -- SetScale _in_a _in_scale
            P.send _out_out ?out_out
