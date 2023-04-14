module Toolkit.Hydra2.Family.Source.FShape where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "shape"


name :: String
name = "shape"


type State = Unit


defaultState :: State
defaultState = unit


_in_sides     = Fn.Input 1 :: _ "sides"
_in_radius    = Fn.Input 2 :: _ "radius"
_in_smoothing = Fn.Input 3 :: _ "smoothing"

_out_out = Fn.Output 1 :: _ "out"


type Inputs = ( sides :: H.Value, radius :: H.Value, smoothing :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_sides _in_radius _in_smoothing


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { sides : H.Number 60.0, radius : H.Number 0.3, smoothing : H.Number 0.01 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> source <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> source <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            sides <- P.receive _in_sides
            radius <- P.receive _in_radius
            smoothing <- P.receive _in_smoothing
            P.send _out_out $ H.From $ H.Shape { sides, radius, smoothing }


type Node (m :: Type -> Type) =
    N.Node "shape" State
        Inputs
        Outputs
        m