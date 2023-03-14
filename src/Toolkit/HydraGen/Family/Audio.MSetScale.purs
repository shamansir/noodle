module Toolkit.HydraGen.Family.Audio.FSetScale where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_scale = Fn.Input :: _ "scale"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: H.Audio, scale :: H.Value )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, scale : H.10 }
        { out : ?out_default }
        $ Fn.make "setScale" $ do
            a <- P.receive _in_a
            scale <- P.receive _in_scale
            -- SetScale a scale
            P.send _out_out ?out_out
