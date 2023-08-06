module Toolkit.Hydra2.Family.Audio.FFft where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "fft"


name :: String
name = "fft"


type State = Unit


defaultState :: State
defaultState = unit


_in_bin   = Fn.Input  0 :: _ "bin"

_out_out  = Fn.Output 0 :: _ "out"


type Inputs = ( bin :: H.AudioBin )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = s1 _in_bin


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { bin : H.AudioBin 0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.None }


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
            bin <- P.receive _in_bin
            -- Fft a h
            P.send _out_out $ H.Fft bin


type Node (m :: Type -> Type) =
    N.Node "fft" State
        Inputs
        Outputs
        m