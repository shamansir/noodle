module Toolkit.HydraGen.Family.Geometry.FKaleid where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, nSides :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, nSides : H.3 }
        { out : ?out_default }
        $ Fn.make "kaleid" $ do
            what <- P.receive _in_what
            nSides <- P.receive _in_nSides
            -- Kaleid what nSides
            P.send _out_out ?out_out
