module Toolkit.Hydra2.Family.Audio.FShow where


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


id = Node.Family :: _ "show"


name :: String
name = "show"


type State = Unit


defaultState :: State
defaultState = unit


_in_audio = Fn.Input  0 :: _ "audio"

_out_out  = Fn.Output 0 :: _ "out"


type Inputs = ( audio :: H.Audio, todo :: H.TODO )
type Outputs = ( out :: H.TODO )


inputsOrder :: _
inputsOrder = s1 _in_audio


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.TODO }


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
            --
            -- Hide a ?input
            -- P.send _out_out ?out_out
            pure unit


type Node (m :: Type -> Type) =
    N.Node "show" State
        Inputs
        Outputs
        m