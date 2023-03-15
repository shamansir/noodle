module Toolkit.HydraGen.Family.Geometry.FScroll where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speedX = Fn.Input :: _ "speedX"
_in_speedY = Fn.Input :: _ "speedY"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, scrollX :: H.Value, scrollY :: H.Value, speedX :: H.Value, speedY :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : H.Empty, scrollX : H.Number 0.5, scrollY : H.Number 0.5, speedX : H.Number 1.0, speedY : H.Number 1.0 }
        { out : H.Empty }
        $ Fn.make "scroll" $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            scrollY <- P.receive _in_scrollY
            speedX <- P.receive _in_speedX
            speedY <- P.receive _in_speedY
            P.send _out_out $ H.Geometry what $ H.GScroll { scrollX, scrollY, speedX, speedY }
