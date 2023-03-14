module Toolkit.HydraGen.Family.Audio.FFft where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_h = Fn.Input :: _ "h"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: H.Audio, h :: H.AudioBin )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, h : ?h_default }
        { out : ?out_default }
        $ Fn.make "fft" $ do
            a <- P.receive _in_a
            h <- P.receive _in_h
            -- Fft a h
            P.send _out_out ?out_out
