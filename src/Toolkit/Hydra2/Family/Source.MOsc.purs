module Toolkit.Hydra2.Family.Source.FOsc where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "osc"


name :: String
name = "osc"


_in_frequency = Fn.Input :: _ "frequency"
_in_sync = Fn.Input :: _ "sync"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Inputs = ( frequency :: H.Value, sync :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { frequency : H.Number 60.0, sync : H.Number 0.1, offset : H.Number 0.0 }


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
            frequency <- P.receive _in_frequency
            sync <- P.receive _in_sync
            offset <- P.receive _in_offset
            P.send _out_out $ H.From $ H.Osc { frequency, sync, offset }


type Node (m :: Type -> Type) =
    N.Node "osc" Unit
        Inputs
        Outputs
        m