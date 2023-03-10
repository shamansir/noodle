module MFft where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_h = Fn.Input :: _ "h"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, h :: AudioBin )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, h : ?h_default }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            h <- P.receive _in_h
            -- Fft _in_a _in_h
            P.send _out_out ?out_out
