module Toolkit.Hydra2.Family.Source.FGradient where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "gradient"


name :: String
name = "gradient"


type State = Unit


defaultState :: State
defaultState = unit


_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Inputs = ( speed :: H.Value )
type Outputs = ( out :: H.Texture )


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
        $ Fn.make name $ do
            speed <- P.receive _in_speed
            P.send _out_out $ H.From $ H.Gradient { speed }


type Node (m :: Type -> Type) =
    N.Node "gradient" State
        Inputs
        Outputs
        m