module Toolkit.HydraGen.Family.Array.FFast where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)

import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: H.VArray, speed :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : H.noValues, speed : H.Number 1.0 }
        { out : H.None }
        $ Fn.make "fast" $ do
            a <- P.receive _in_a
            speed <- P.receive _in_speed
            P.send _out_out $ H.VArray a $ H.Fast speed
