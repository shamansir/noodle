module Toolkit.HydraGen.Family.Array.FFast where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)

import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_arr = Fn.Input :: _ "arr"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( arr :: H.VArray, speed :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { arr : H.noValues, speed : H.Number 1.0 }
        { out : H.None }
        $ Fn.make "fast" $ do
            arr <- P.receive _in_arr
            speed <- P.receive _in_speed
            P.send _out_out $ H.VArray arr $ H.Fast speed
