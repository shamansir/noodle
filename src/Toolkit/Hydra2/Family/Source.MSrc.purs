module Toolkit.Hydra2.Family.Source.FSrc where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_src = Fn.Input :: _ "src"

_out_out = Fn.Output :: _ "out"


type Inputs = ( src :: H.From )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { src : H.All }


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
        $ Fn.make "src" $ do
            src <- P.receive _in_src
            P.send _out_out $ H.From $ H.Source src


type Node m =
    N.Node "src" Unit
        Inputs
        Outputs
        m