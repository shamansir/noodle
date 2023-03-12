module Toolkit.HydraGen.Family.Array.FFit where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_low = Fn.Input :: _ "low"
_in_high = Fn.Input :: _ "high"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, low :: Value, high :: Value )
        ( out :: Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, low : 0, high : 1 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            low <- P.receive _in_low
            high <- P.receive _in_high
            -- Fit a low high
            P.send _out_out ?out_out
