module Toolkit.HydraGen.Family.Color.FShift where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_r = Fn.Input :: _ "r"
_in_g = Fn.Input :: _ "g"
_in_b = Fn.Input :: _ "b"
_in_a = Fn.Input :: _ "a"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, r :: Value, g :: Value, b :: Value, a :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, r : 0.5, g : ?g_default, b : ?b_default, a : ?a_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            -- Shift what r g b a
            P.send _out_out ?out_out
