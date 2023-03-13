module Toolkit.HydraGen.Family.Geometry.FScroll where





import Prelude
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
        ( what :: Texture, scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, scrollX : 0.5, scrollY : 0.5, speedX : ?speedX_default, speedY : ?speedY_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            scrollY <- P.receive _in_scrollY
            speedX <- P.receive _in_speedX
            speedY <- P.receive _in_speedY
            -- Scroll what scrollX scrollY speedX speedY
            P.send _out_out ?out_out
