module Toolkit.HydraGen.Family.Synth.FRender where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_from = Fn.Input :: _ "from"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( from :: H.From )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { from : H.All }
        { out : ?out_default }
        $ Fn.make "render" $ do
            from <- P.receive _in_from
            -- Render from
            P.send _out_out ?out_out
