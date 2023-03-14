module Toolkit.HydraGen.Family.Array.FFit where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_low = Fn.Input :: _ "low"
_in_high = Fn.Input :: _ "high"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: H.Array, low :: H.Value, high :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, low : H.0, high : H.1 }
        { out : ?out_default }
        $ Fn.make "fit" $ do
            a <- P.receive _in_a
            low <- P.receive _in_low
            high <- P.receive _in_high
            -- Fit a low high
            P.send _out_out ?out_out
