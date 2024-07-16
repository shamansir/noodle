module Toolkit.Hydra.Family.ExternalSources.FInitCam where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


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


id = Node.Family :: _ "initCam"


name :: String
name = "initCam"


type State = Unit


defaultState :: State
defaultState = unit


_in_src   = Fn.Input 0 :: _ "src"
_in_index = Fn.Input 1 :: _ "index"


type Inputs = ( src :: H.SourceN, index :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_src _in_index


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSourceN, index : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> extsource <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
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
            index <- P.receive _in_index
            pure unit


type Node (m :: Type -> Type) =
    N.Node "initCam" State
        Inputs
        Outputs
        WrapRepr
        m