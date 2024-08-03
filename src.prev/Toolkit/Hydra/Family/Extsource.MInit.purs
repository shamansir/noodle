module Toolkit.Hydra.Family.ExternalSources.FInit where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "init"


name :: String
name = "init"


type State = Unit


defaultState :: State
defaultState = unit


_in_options = Fn.Input 0 :: _ "options"


type Inputs = ( options :: H.SourceOptions )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_options


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { options : H.defaultSourceOptions }


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
            options <- P.receive _in_options
            pure unit


type Node (m :: Type -> Type) =
    N.Node "init" State
        Inputs
        Outputs
        WrapRepr
        m