module Toolkit.Hydra2.Family.Geometry.FScroll where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_what = Fn.Input :: _ "what"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speedX = Fn.Input :: _ "speedX"
_in_speedY = Fn.Input :: _ "speedY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, scrollX :: H.Value, scrollY :: H.Value, speedX :: H.Value, speedY :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, scrollX : H.Number 0.5, scrollY : H.Number 0.5, speedX : H.Number 1.0, speedY : H.Number 1.0 }


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
        $ Fn.make "scroll" $ do
            what <- P.receive _in_what
            scrollX <- P.receive _in_scrollX
            scrollY <- P.receive _in_scrollY
            speedX <- P.receive _in_speedX
            speedY <- P.receive _in_speedY
            P.send _out_out $ H.Geometry what $ H.GScroll { scrollX, scrollY, speedX, speedY }


type Node m =
    N.Node "scroll" Unit
        Inputs
        Outputs
        m