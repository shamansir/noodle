module MScale where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"
_in_xMult = Fn.Input :: _ "xMult"
_in_yMult = Fn.Input :: _ "yMult"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, amount :: Value, xMult :: Value, yMult :: Value, offsetX :: Value, offsetY :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, amount : 1.5, xMult : 1, yMult : 1, offsetX : 0.5, offsetY : 0.5 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            xMult <- P.receive _in_xMult
            yMult <- P.receive _in_yMult
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- Scale _in_what _in_amount _in_xMult _in_yMult _in_offsetX _in_offsetY
            P.send _out_out ?out_out
