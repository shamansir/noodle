module Toolkit.HydraGen.Family.Audio.FSetScale where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"
_in_scale = Fn.Input :: _ "scale"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, scale :: H.Value )
        ( )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, scale : H.Number 10.0 }
        { }
        $ Fn.make "setScale" $ do
            audio <- P.receive _in_audio
            scale <- P.receive _in_scale
            pure unit
