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


_in_audio = Fn.Input  1 :: _ "audio"
_in_h     = Fn.Input  2 :: _ "h"

_out_out  = Fn.Output 1 :: _ "out"


type Inputs = ( audio :: H.Audio, h :: H.AudioBin )
type Outputs = ( out :: H.Value )


inputsOrder :: _
inputsOrder = s2 _in_audio _in_h


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, h : H.H0 }


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
            audio <- P.receive _in_audio
            h <- P.receive _in_h
            -- Fft a h
            P.send _out_out $ H.Audio audio h


type Node (m :: Type -> Type) =
    N.Node "fft" State
        Inputs
        Outputs
        m