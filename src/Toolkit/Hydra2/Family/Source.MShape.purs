module Toolkit.Hydra2.Family.Source.FShape where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_sides = Fn.Input :: _ "sides"
_in_radius = Fn.Input :: _ "radius"
_in_smoothing = Fn.Input :: _ "smoothing"

_out_out = Fn.Output :: _ "out"


type Inputs = ( sides :: H.Value, radius :: H.Value, smoothing :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { sides : H.Number 60.0, radius : H.Number 0.3, smoothing : H.Number 0.01 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family m = -- {-> source <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "shape" $ do
            sides <- P.receive _in_sides
            radius <- P.receive _in_radius
            smoothing <- P.receive _in_smoothing
            P.send _out_out $ H.From $ H.Shape { sides, radius, smoothing }


type Node m =
    N.Node "shape" Unit
        Inputs
        Outputs
        m