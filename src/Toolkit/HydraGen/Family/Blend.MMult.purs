module Blend.MMult where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> blend <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, amount :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> blend <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, amount : 1 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            amount <- P.receive _in_amount
            -- Mult what with amount
            P.send _out_out ?out_out
