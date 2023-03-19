module Toolkit.Hydra2.Family.Array.FEase where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "ease"


name :: String
name = "ease"


type State = Unit


defaultState :: State
defaultState = unit


_in_arr = Fn.Input :: _ "arr"
_in_ease = Fn.Input :: _ "ease"

_out_out = Fn.Output :: _ "out"


type Inputs = ( arr :: H.VArray, ease :: H.Ease )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { arr : H.noValues, ease : H.Linear }


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
        { arr : H.noValues, ease : H.Linear }
        { out : H.None }
        $ Fn.make name $ do
            arr <- P.receive _in_arr
            ease <- P.receive _in_ease
            P.send _out_out $ H.VArray arr ease


type Node (m :: Type -> Type) =
    N.Node "ease" State
        Inputs
        Outputs
        m