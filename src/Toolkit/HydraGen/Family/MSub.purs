module MSub where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> blend <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, amount :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> blend <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, amount : 1 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            amount <- P.receive _in_amount
            -- Sub _in_what _in_with _in_amount
            P.send _out_out ?out_out
