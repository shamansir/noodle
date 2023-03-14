module Toolkit.HydraGen.Family.Synth.FSpeed where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_v = Fn.Input :: _ "v"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( v :: H.Value )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { v : H.1 }
        { out : ?out_default }
        $ Fn.make "speed" $ do
            v <- P.receive _in_v
            -- Speed v
            P.send _out_out ?out_out
