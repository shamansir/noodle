module Toolkit.Hydra2.Family.Audio.FShow where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_audio = Fn.Input :: _ "audio"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( audio :: H.Audio, todo :: H.TODO )
        ( out :: H.TODO )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { audio : H.Silence, todo : H.TODO }
        { out : H.TODO }
        $ Fn.make "show" $ do
            audio <- P.receive _in_audio
            --
            -- Hide a ?input
            -- P.send _out_out ?out_out
            pure unit
