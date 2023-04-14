module Toolkit.Hydra2.Family.Audio.FSetSmooth where


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


id = Node.Family :: _ "setSmooth"


name :: String
name = "setSmooth"


type State = Unit


defaultState :: State
defaultState = unit


_in_audio  = Fn.Input 1 :: _ "audio"
_in_smooth = Fn.Input 2 :: _ "smooth"


type Inputs = ( audio :: H.Audio, smooth :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_audio _in_smooth


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, smooth : H.Number 0.4 }


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
            smooth <- P.receive _in_smooth
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setSmooth" State
        Inputs
        Outputs
        m