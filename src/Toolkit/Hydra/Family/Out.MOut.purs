module Toolkit.Hydra.Family.Out.FOut where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "out"


name :: String
name = "out"


type State = H.OutputN


defaultState :: State
defaultState = H.Output0


_in_what   = Fn.Input  0 :: _ "what"
_in_target = Fn.Input  1 :: _ "target"

type Inputs = ( what :: H.Texture, target :: H.OutputN )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_what _in_target


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, target : H.Output0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> out <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> out <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            target <- P.receive _in_target
            pure unit


type Node (m :: Type -> Type) =
    N.Node "out" State
        Inputs
        Outputs
        m