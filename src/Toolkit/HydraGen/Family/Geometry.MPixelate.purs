module Toolkit.HydraGen.Family.Geometry.FPixelate where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_pixelX = Fn.Input :: _ "pixelX"
_in_pixelY = Fn.Input :: _ "pixelY"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, pixelX :: H.Value, pixelY :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, pixelX : H.20, pixelY : H.20 }
        { out : ?out_default }
        $ Fn.make "pixelate" $ do
            what <- P.receive _in_what
            pixelX <- P.receive _in_pixelX
            pixelY <- P.receive _in_pixelY
            -- Pixelate what pixelX pixelY
            P.send _out_out ?out_out
