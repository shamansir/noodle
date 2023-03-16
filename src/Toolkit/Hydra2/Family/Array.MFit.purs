module Toolkit.Hydra2.Family.Array.FFit where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_arr = Fn.Input :: _ "arr"
_in_low = Fn.Input :: _ "low"
_in_high = Fn.Input :: _ "high"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( arr :: H.VArray, low :: H.Value, high :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { arr : H.noValues, low : H.Number 0.0, high : H.Number 1.1 }
        { out : H.None }
        $ Fn.make "fit" $ do
            arr <- P.receive _in_arr
            low <- P.receive _in_low
            high <- P.receive _in_high
            P.send _out_out $ H.VArray arr $ H.Fit low high
