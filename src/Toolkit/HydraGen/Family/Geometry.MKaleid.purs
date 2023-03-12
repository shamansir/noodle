module Toolkit.HydraGen.Family.Geometry.FKaleid where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, nSides :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, nSides : 3 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            nSides <- P.receive _in_nSides
            -- Kaleid what nSides
            P.send _out_out ?out_out
