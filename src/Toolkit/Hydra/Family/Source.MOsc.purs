module Toolkit.Hydra.Family.Source.FOsc where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure, discard, show)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console


id = Node.Family :: _ "osc"


name :: String
name = "osc"


type State = Unit


defaultState :: State
defaultState = unit


_in_frequency = Fn.Input  0 :: _ "frequency"
_in_sync      = Fn.Input  1 :: _ "sync"
_in_offset    = Fn.Input  2 :: _ "offset"

_out_out      = Fn.Output 0 :: _ "out"


type Inputs = ( frequency :: H.Value, sync :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_frequency _in_sync _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { frequency : H.Number 60.0, sync : H.Number 0.1, offset : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> source <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). MonadEffect m => Family m
family = -- {-> source <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            frequency <- P.receive _in_frequency
            sync <- P.receive _in_sync
            offset <- P.receive _in_offset
            P.send _out_out $ H.Start $ H.Osc { frequency, sync, offset }


type Node (m :: Type -> Type) =
    N.Node "osc" State
        Inputs
        Outputs
        m