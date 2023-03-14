module Toolkit.HydraGen.Family.Geometry.FScale where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"
_in_xMult = Fn.Input :: _ "xMult"
_in_yMult = Fn.Input :: _ "yMult"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, amount :: H.Value, xMult :: H.Value, yMult :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, amount : H.1.5, xMult : H.1, yMult : H.1, offsetX : H.0.5, offsetY : H.0.5 }
        { out : ?out_default }
        $ Fn.make "scale" $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            xMult <- P.receive _in_xMult
            yMult <- P.receive _in_yMult
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- Scale what amount xMult yMult offsetX offsetY
            P.send _out_out ?out_out
