module Toolkit.HydraGen.Family.Color.FColor where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_r = Fn.Input :: _ "r"
_in_g = Fn.Input :: _ "g"
_in_b = Fn.Input :: _ "b"
_in_a = Fn.Input :: _ "a"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( r :: H.Value, g :: H.Value, b :: H.Value, a :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { r : H.1, g : H.1, b : H.1, a : H.1 }
        { out : ?out_default }
        $ Fn.make "color" $ do
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            -- Color r g b a
            P.send _out_out ?out_out
