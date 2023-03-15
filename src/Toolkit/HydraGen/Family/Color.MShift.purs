module Toolkit.HydraGen.Family.Color.FShift where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
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
        ( what :: H.Texture, r :: H.Value, g :: H.Value, b :: H.Value, a :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, r : H.Number 0.5, g : H.Number 0.5, b : H.Number 0.5, a : H.Number 0.5 }
        { out : H.Empty }
        $ Fn.make "shift" $ do
            what <- P.receive _in_what
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            P.send _out_out $ H.WithColor what $ H.Shift { r, g, b, a }
