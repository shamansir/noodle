module Toolkit.HydraGen.Family.Audio.FSetSmooth where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"
_in_smooth = Fn.Input :: _ "smooth"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, smooth :: H.Value )
        ( )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, smooth : H.Number 0.4 }
        { }
        $ Fn.make "setSmooth" $ do
            audio <- P.receive _in_audio
            smooth <- P.receive _in_smooth
            pure unit
