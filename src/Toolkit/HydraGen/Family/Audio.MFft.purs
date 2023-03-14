module Toolkit.HydraGen.Family.Audio.FFft where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"
_in_h = Fn.Input :: _ "h"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, h :: H.AudioBin )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, h : H.H0 }
        { out : H.None }
        $ Fn.make "fft" $ do
            audio <- P.receive _in_audio
            h <- P.receive _in_h
            -- Fft a h
            P.send _out_out $ H.Audio audio h
