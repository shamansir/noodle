module Toolkit.Hydra2.Family.Geometry.FKaleid where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_what = Fn.Input :: _ "what"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, nSides :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, nSides : H.Number 3.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family m = -- {-> geometry <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "kaleid" $ do
            what <- P.receive _in_what
            nSides <- P.receive _in_nSides
            P.send _out_out $ H.Geometry what $ H.GKaleid { nSides }


type Node m =
    N.Node "kaleid" Unit
        Inputs
        Outputs
        m