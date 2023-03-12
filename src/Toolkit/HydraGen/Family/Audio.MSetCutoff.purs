module Toolkit.HydraGen.Family.Audio.FSetCutoff where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_cutoff = Fn.Input :: _ "cutoff"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, cutoff :: Value )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, cutoff : 2 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            cutoff <- P.receive _in_cutoff
            -- SetCutoff a cutoff
            P.send _out_out ?out_out
