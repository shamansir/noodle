module Toolkit.Hydra.Family.Source.FGradient where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "gradient"


name :: String
name = "gradient"


type State = Unit


defaultState :: State
defaultState = unit


_in_speed = Fn.Input  0 :: _ "speed"

_out_out  = Fn.Output 0 :: _ "out"


type Inputs = ( speed :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s1 _in_speed


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { speed : H.Number 1.0 }


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
            speed <- P.receive _in_speed
            P.send _out_out $ H.Start $ H.Gradient { speed }


type Node (m :: Type -> Type) =
    N.Node "gradient" State
        Inputs
        Outputs
        m