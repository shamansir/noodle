module Toolkit.Hydra2.Family.Synth.FRender where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_from = Fn.Input :: _ "from"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( from :: H.From )
        ( )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { from : H.All }
        { }
        $ Fn.make "render" $ do
            from <- P.receive _in_from
            -- TODO
            pure unit
