module Toolkit.HydraGen.Family.Source.FShape where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_sides = Fn.Input :: _ "sides"
_in_radius = Fn.Input :: _ "radius"
_in_smoothing = Fn.Input :: _ "smoothing"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( sides :: H.Value, radius :: H.Value, smoothing :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { sides : H.Number 60.0, radius : H.Number 0.3, smoothing : H.Number 0.01 }
        { out : H.Empty }
        $ Fn.make "shape" $ do
            sides <- P.receive _in_sides
            radius <- P.receive _in_radius
            smoothing <- P.receive _in_smoothing
            P.send _out_out $ H.From $ H.Shape { sides, radius, smoothing }
