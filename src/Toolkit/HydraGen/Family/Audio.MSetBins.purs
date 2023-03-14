module Toolkit.HydraGen.Family.Audio.FSetBins where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"
_in_numBins = Fn.Input :: _ "numBins"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, numBins :: H.Value )
        ( )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, numBins : H.Number 4.0 }
        { }
        $ Fn.make "setBins" $ do
            audio <- P.receive _in_audio
            numBins <- P.receive _in_numBins
            -- TODO
            pure unit
