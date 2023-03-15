module Toolkit.HydraGen.Family.Geometry.FScrollX where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, scrollX :: H.Value, speed :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : H.Empty, scrollX : H.Number 0.5, speed : H.Number 1.0 }
        { out : H.Empty }
        $ Fn.make "scrollX" $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            P.send _out_out $ H.Geometry what $ H.GScrollX { scrollX, speed }
