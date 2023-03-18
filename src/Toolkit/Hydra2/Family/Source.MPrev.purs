module Toolkit.Hydra2.Family.Source.FPrev where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N



_out_out = Fn.Output :: _ "out"


type Inputs = ( todo :: H.TODO )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { todo : H.TODO }


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
        $ Fn.make "prev" $ do
            P.send _out_out $ H.Empty


type Node m =
    N.Node "prev" Unit
        Inputs
        Outputs
        m