module Toolkit.HydraGen.Family.Synth.FRender where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_from = Fn.Input :: _ "from"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( from :: From )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { from : All }
        { out : ?out_default }
        $ Fn.make $ do
            from <- P.receive _in_from
            -- Render from
            P.send _out_out ?out_out
