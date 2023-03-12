module Toolkit.HydraGen.Family.Array.FSmooth where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, smooth :: Value )
        ( out :: Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, smooth : 1 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            smooth <- P.receive _in_smooth
            -- Smooth a smooth
            P.send _out_out ?out_out
