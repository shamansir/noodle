module Toolkit.HydraGen.Family.Synth.FSetFunction where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_fn = Fn.Input :: _ "fn"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( fn :: H.GlslFn )
        ( )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { fn : H.defaultGlslFn }
        { }
        $ Fn.make "setFunction" $ do
            fn <- P.receive _in_fn
            -- TODO
            pure unit
