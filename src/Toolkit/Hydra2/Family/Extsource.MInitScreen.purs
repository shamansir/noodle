module Toolkit.Hydra2.Family.ExternalSources.FInitScreen where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "initScreen"


name :: String
name = "initScreen"


type State = Unit


defaultState :: State
defaultState = unit


_in_src = Fn.Input 0 :: _ "src"


type Inputs = ( src :: H.Source )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_src


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSource }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> extsource <-}
    Family.Def State
        Inputs
        Outputs
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> extsource <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            src <- P.receive _in_src
            pure unit


type Node (m :: Type -> Type) =
    N.Node "initScreen" State
        Inputs
        Outputs
        m