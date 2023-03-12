module Toolkit.HydraGen.Family.Audio.FSetSmooth where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, smooth :: Value )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, smooth : 0.4 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            smooth <- P.receive _in_smooth
            -- SetSmooth a smooth
            P.send _out_out ?out_out
