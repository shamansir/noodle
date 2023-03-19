module Toolkit.Hydra2.Family.Source.FNoise where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "noise"


name :: String
name = "noise"


_in_scale = Fn.Input :: _ "scale"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Inputs = ( scale :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { scale : H.Number 10.0, offset : H.Number 0.1 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> source <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> source <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            P.send _out_out $ H.From $ H.Noise { scale, offset }


type Node (m :: Type -> Type) =
    N.Node "noise" Unit
        Inputs
        Outputs
        m