module Toolkit.HydraGen.Family.Audio.FSetCutoff where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_cutoff = Fn.Input :: _ "cutoff"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: H.Audio, cutoff :: H.Value )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, cutoff : H.2 }
        { out : ?out_default }
        $ Fn.make "setCutoff" $ do
            a <- P.receive _in_a
            cutoff <- P.receive _in_cutoff
            -- SetCutoff a cutoff
            P.send _out_out ?out_out
