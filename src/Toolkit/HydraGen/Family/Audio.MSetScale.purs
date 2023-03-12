module Toolkit.HydraGen.Family.Audio.FSetScale where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_scale = Fn.Input :: _ "scale"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, scale :: Value )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, scale : 10 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            scale <- P.receive _in_scale
            -- SetScale a scale
            P.send _out_out ?out_out
