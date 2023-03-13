module Toolkit.HydraGen.Family.Array.FOffset where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, offset :: Value )
        ( out :: Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, offset : 0.5 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            offset <- P.receive _in_offset
            -- Offset a offset
            P.send _out_out ?out_out
