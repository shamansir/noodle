module Toolkit.HydraGen.Family.Array.FEase where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_arr = Fn.Input :: _ "arr"
_in_ease = Fn.Input :: _ "ease"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( arr :: H.VArray, ease :: H.Ease )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { arr : H.noValues, ease : H.Linear }
        { out : H.None }
        $ Fn.make "ease" $ do
            arr <- P.receive _in_arr
            ease <- P.receive _in_ease
            P.send _out_out $ H.VArray arr ease
