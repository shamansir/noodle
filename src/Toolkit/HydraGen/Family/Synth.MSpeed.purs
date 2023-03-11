module Synth.MSpeed where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_v = Fn.Input :: _ "v"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( v :: Value )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { v : 1 }
        { out : ?out_default }
        $ Fn.make $ do
            v <- P.receive _in_v
            -- Speed v
            P.send _out_out ?out_out
