module Toolkit.Hydra2.Family.Array.FSmooth where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "smooth"


name :: String
name = "smooth"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr = Fn.Input :: _ "arr"
_in_smooth = Fn.Input :: _ "smooth"

_out_out = Fn.Output :: _ "out"


type Inputs = ( arr :: H.VArray, smooth :: H.Value )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, smooth : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.None }


type Family (m :: Type -> Type) = -- {-> array <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> array <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            arr <- P.receive _in_arr
            smooth <- P.receive _in_smooth
            P.send _out_out $ H.VArray arr $ H.Smooth smooth


type Node (m :: Type -> Type) =
    N.Node "smooth" State
        Inputs
        Outputs
        m