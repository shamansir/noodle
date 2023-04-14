module Toolkit.Hydra2.Family.Audio.FSetCutoff where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "setCutoff"


name :: String
name = "setCutoff"


type State = Unit


defaultState :: State
defaultState = unit


_in_audio  = Fn.Input 1 :: _ "audio"
_in_cutoff = Fn.Input 1 :: _ "cutoff"


type Inputs = ( audio :: H.Audio, cutoff :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_audio _in_cutoff


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, cutoff : H.Number 2.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> audio <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> audio <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            audio <- P.receive _in_audio
            cutoff <- P.receive _in_cutoff
            -- SetCutoff a cutoff
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setCutoff" State
        Inputs
        Outputs
        m