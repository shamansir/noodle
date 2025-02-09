module Toolkit.Hydra.Family.Color.FLuma where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "luma"


name :: String
name = "luma"


type State = Unit


defaultState :: State
defaultState = unit


_in_what      = Fn.Input  0 :: _ "what"
_in_threshold  = Fn.Input  1 :: _ "threshold"
_in_tolerance = Fn.Input  2 :: _ "tolerance"

_out_out      = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, threshold :: H.Value, tolerance :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_threshold _in_tolerance


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, threshold : H.Number 0.5, tolerance : H.Number 0.1 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            threshold <- P.receive _in_threshold
            tolerance <- P.receive _in_tolerance
            P.send _out_out $ H.Filter what $ H.Luma { threshold, tolerance }


type Node (m :: Type -> Type) =
    N.Node "luma" State
        Inputs
        Outputs
        WrapRepr
        m