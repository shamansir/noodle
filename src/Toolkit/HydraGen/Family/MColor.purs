module MColor where

import Prelude

_in_r = Fn.Input :: _ "r"
_in_g = Fn.Input :: _ "g"
_in_b = Fn.Input :: _ "b"
_in_a = Fn.Input :: _ "a"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( r :: Value, g :: Value, b :: Value, a :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { r : 1, g : 1, b : 1, a : 1 }
        { out : ?out_default }
        $ fml.make $ do
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            -- Color _in_r _in_g _in_b _in_a
            P.send _out_out ?out_out
