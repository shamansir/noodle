module Toolkit.Hydra2.Family.Modulate.FModulateRepeatX where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s4)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "modulateRepeatX"


name :: String
name = "modulateRepeatX"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  1 :: _ "what"
_in_with   = Fn.Input  2 :: _ "with"
_in_reps   = Fn.Input  3 :: _ "reps"
_in_offset = Fn.Input  4 :: _ "offset"

_out_out   = Fn.Output 1 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, reps :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s4 _in_what _in_with _in_reps _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, reps : H.Number 3.0, offset : H.Number 0.5 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModRepeatX { reps, offset }


type Node (m :: Type -> Type) =
    N.Node "modulateRepeatX" State
        Inputs
        Outputs
        m