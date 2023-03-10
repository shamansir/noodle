module MRepeat where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_repeatX = Fn.Input :: _ "repeatX"
_in_repeatY = Fn.Input :: _ "repeatY"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, repeatX : 3, repeatY : 3, offsetX : ?offsetX_default, offsetY : ?offsetY_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- Repeat _in_what _in_repeatX _in_repeatY _in_offsetX _in_offsetY
            P.send _out_out ?out_out
