module Toolkit.HydraGen.Family.Array.FOffset where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: H.Array, offset :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, offset : H.0.5 }
        { out : ?out_default }
        $ Fn.make "offset" $ do
            a <- P.receive _in_a
            offset <- P.receive _in_offset
            -- Offset a offset
            P.send _out_out ?out_out
