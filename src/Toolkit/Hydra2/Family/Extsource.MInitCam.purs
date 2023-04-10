module Toolkit.Hydra2.Family.Extsource.FInitCam where


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


id = Node.Family :: _ "initCam"


name :: String
name = "initCam"


type State = Unit


defaultState :: State
defaultState = unit


_in_src = Fn.Input :: _ "src"
_in_index = Fn.Input :: _ "index"


type Inputs = ( src :: H.Source, index :: H.Value )
type Outputs = ( )


type InputsOrder :: SOrder
type InputsOrder = "src" ::: "index" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = T


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSource, index : H.Number 0.0 }


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
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            src <- P.receive _in_src
            index <- P.receive _in_index
            pure unit


type Node (m :: Type -> Type) =
    N.Node "initCam" State
        Inputs
        Outputs
        m