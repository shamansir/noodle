module Toolkit.Hydra2.Family.Source.FSrc where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "src"


name :: String
name = "src"


type State = Unit


defaultState :: State
defaultState = unit


_in_src = Fn.Input :: _ "src"

_out_out = Fn.Output :: _ "out"


type Inputs = ( src :: H.From )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { src : H.All }


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
        $ Fn.make name $ do
            src <- P.receive _in_src
            P.send _out_out $ H.From $ H.Source src


type Node (m :: Type -> Type) =
    N.Node "src" State
        Inputs
        Outputs
        m