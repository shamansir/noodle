module MInvert where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, amount :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, amount : 1 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            -- Invert _in_what _in_amount
            P.send _out_out ?out_out
