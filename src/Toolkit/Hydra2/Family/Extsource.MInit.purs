module Toolkit.Hydra2.Family.Extsource.FInit where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "init"


name :: String
name = "init"


type State = Unit


defaultState :: State
defaultState = unit


_in_options = Fn.Input :: _ "options"


type Inputs = ( options :: H.SourceOptions )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { options : { src : H.Canvas } }


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
            options <- P.receive _in_options
            pure unit


type Node (m :: Type -> Type) =
    N.Node "init" State
        Inputs
        Outputs
        m