module Toolkit.HydraGen.Family.Synth.FSpeed where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_v = Fn.Input :: _ "v"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( v :: H.Value )
        ( )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { v : H.Number 1.0 }
        { }
        $ Fn.make "speed" $ do
            v <- P.receive _in_v
            -- TODO
            pure unit
