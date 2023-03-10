module MSolid where

import Prelude

_in_r = Fn.Input :: _ "r"
_in_g = Fn.Input :: _ "g"
_in_b = Fn.Input :: _ "b"
_in_a = Fn.Input :: _ "a"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( r :: Value, g :: Value, b :: Value, a :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { r : ?r_default, g : ?g_default, b : ?b_default, a : 1 }
        { out : ?out_default }
        $ fml.make $ do
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            -- Solid _in_r _in_g _in_b _in_a
            P.send _out_out ?out_out
