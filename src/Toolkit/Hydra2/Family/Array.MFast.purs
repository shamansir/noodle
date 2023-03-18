module Toolkit.Hydra2.Family.Array.FFast where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)

import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "fast"


name :: String
name = "fast"


_in_arr = Fn.Input :: _ "arr"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Inputs = ( arr :: H.VArray, speed :: H.Value )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, speed : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.None }


type Family m = -- {-> array <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            arr <- P.receive _in_arr
            speed <- P.receive _in_speed
            P.send _out_out $ H.VArray arr $ H.Fast speed


type Node m =
    N.Node "fast" Unit
        Inputs
        Outputs
        m
