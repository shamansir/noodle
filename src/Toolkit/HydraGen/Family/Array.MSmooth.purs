module Toolkit.HydraGen.Family.Array.FSmooth where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_arr = Fn.Input :: _ "arr"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( arr :: H.VArray, smooth :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { arr : H.noValues, smooth : H.Number 1.0 }
        { out : H.None }
        $ Fn.make "smooth" $ do
            a <- P.receive _in_arr
            smooth <- P.receive _in_smooth
            P.send _out_out $ H.VArray a $ H.Smooth smooth
