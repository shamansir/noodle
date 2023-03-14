module Toolkit.HydraGen.Family.Audio.FSetCutoff where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"
_in_cutoff = Fn.Input :: _ "cutoff"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, cutoff :: H.Value )
        ( )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, cutoff : H.Number 2.0 }
        { }
        $ Fn.make "setCutoff" $ do
            audio <- P.receive _in_audio
            cutoff <- P.receive _in_cutoff
            -- SetCutoff a cutoff
            pure unit
