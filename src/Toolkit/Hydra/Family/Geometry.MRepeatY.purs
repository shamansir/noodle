module Toolkit.Hydra.Family.Geometry.FRepeatY where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s3, s1)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "repeatY"


name :: String
name = "repeatY"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_reps   = Fn.Input  1 :: _ "reps"
_in_offset = Fn.Input  2 :: _ "offset"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, reps :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_reps _in_offset


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, reps : H.Number 3.0, offset : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> geometry <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- RepeatY what reps offset
            P.send _out_out $ H.Geometry what $ H.GRepeatY { reps, offset }


type Node (m :: Type -> Type) =
    N.Node "repeatY" State
        Inputs
        Outputs
        WrapRepr
        m