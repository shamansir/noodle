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
        ( a :: H.VArray, offset :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : H.noValues, offset : H.Number 0.5 }
        { out : H.None }
        $ Fn.make "offset" $ do
            a <- P.receive _in_a
            offset <- P.receive _in_offset
            P.send _out_out $ H.VArray a $ H.Offset offset
