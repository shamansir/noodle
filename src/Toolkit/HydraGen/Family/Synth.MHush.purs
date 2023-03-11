module Synth.MHush where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_Unit = Fn.Input :: _ "Unit"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( Unit :: Unknown )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { Unit : ?Unit_default }
        { out : ?out_default }
        $ Fn.make $ do
            Unit <- P.receive _in_Unit
            -- Hush Unit
            P.send _out_out ?out_out
