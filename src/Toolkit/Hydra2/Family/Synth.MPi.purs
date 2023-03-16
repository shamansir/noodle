module Toolkit.Hydra2.Family.Synth.FPi where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family




_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { }
        { out : H.Pi }
        $ Fn.make "pi" $ do
            -- Pi
            P.send _out_out $ H.Pi
