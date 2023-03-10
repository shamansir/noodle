module MSmooth where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, smooth :: Value )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, smooth : 1 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            smooth <- P.receive _in_smooth
            -- Smooth _in_a _in_smooth
            P.send _out_out ?out_out
