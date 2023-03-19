module Toolkit.Hydra2.Family.Synth.FHush where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "hush"


name :: String
name = "hush"


type State = Unit


defaultState :: State
defaultState = unit


_in_todo = Fn.Input :: _ "todo"

_out_out = Fn.Output :: _ "out"


type Inputs = ( todo :: H.TODO )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> synth <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> synth <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            _ <- P.receive _in_todo
            pure unit


type Node (m :: Type -> Type) =
    N.Node "hush" State
        Inputs
        Outputs
        m