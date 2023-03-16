module Toolkit.Hydra2.Family.Synth.FUpdate where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_fn = Fn.Input :: _ "fn"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( fn :: H.UpdateFn )
        ( )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { fn : H.defaultUpdateFn }
        { }
        $ Fn.make "update" $ do
            fn <- P.receive _in_fn
            -- TODO
            pure unit
