module MFit where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_low = Fn.Input :: _ "low"
_in_high = Fn.Input :: _ "high"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, low :: Value, high :: Value )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, low : 0, high : 1 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            low <- P.receive _in_low
            high <- P.receive _in_high
            -- Fit _in_a _in_low _in_high
            P.send _out_out ?out_out
