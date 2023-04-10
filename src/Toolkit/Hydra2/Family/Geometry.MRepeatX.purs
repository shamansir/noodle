module Toolkit.Hydra2.Family.Geometry.FRepeatX where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "repeatX"


name :: String
name = "repeatX"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_reps = Fn.Input :: _ "reps"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, reps :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


type InputsOrder :: SOrder
type InputsOrder = "what" ::: "reps" ::: "offset" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, reps : H.Number 3.0, offset : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def State
        Inputs
        Outputs
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> geometry <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            what <- P.receive _in_what
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- RepeatX what reps offset
            P.send _out_out $ H.Geometry what $ H.GRepeatX { reps, offset }


type Node (m :: Type -> Type) =
    N.Node "repeatX" State
        Inputs
        Outputs
        m