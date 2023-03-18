module Toolkit.Hydra2.Family.Geometry.FRotate where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "rotate"


name :: String
name = "rotate"


_in_what = Fn.Input :: _ "what"
_in_angle = Fn.Input :: _ "angle"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, angle :: H.Value, speed :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, angle : H.Number 10.0, speed : H.Number 1.0 }


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
        $ Fn.make name $ do
            what <- P.receive _in_what
            angle <- P.receive _in_angle
            speed <- P.receive _in_speed
            -- Rotate what angle speed
            P.send _out_out $ H.Geometry what $ H.GRotate { angle, speed }


type Node m =
    N.Node "rotate" Unit
        Inputs
        Outputs
        m