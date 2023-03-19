module Toolkit.Hydra2.Family.Extsource.FInitStream where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "initStream"


name :: String
name = "initStream"


type State = Unit


defaultState :: State
defaultState = unit


_in_src = Fn.Input :: _ "src"


type Inputs = ( src :: H.Source, todo :: H.TODO )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSource, todo : H.TODO }


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
        $ Fn.make name $ do
            src <- P.receive _in_src
            pure unit


type Node (m :: Type -> Type) =
    N.Node "initStream" State
        Inputs
        Outputs
        m