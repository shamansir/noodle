module Toolkit.Hydra2.Family.Source.FPrev where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "prev"


name :: String
name = "prev"


type State = Unit


defaultState :: State
defaultState = unit


_out_out = Fn.Output :: _ "out"


type Inputs = ( todo :: H.TODO )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { todo : H.TODO }


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
            P.send _out_out $ H.Empty


type Node (m :: Type -> Type) =
    N.Node "prev" State
        Inputs
        Outputs
        m