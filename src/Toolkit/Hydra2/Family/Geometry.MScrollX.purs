module Toolkit.Hydra2.Family.Geometry.FScrollX where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, scrollX :: H.Value, speed :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, scrollX : H.Number 0.5, speed : H.Number 1.0 }


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
        $ Fn.make "scrollX" $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            P.send _out_out $ H.Geometry what $ H.GScrollX { scrollX, speed }


type Node m =
    N.Node "scrollX" Unit
        Inputs
        Outputs
        m