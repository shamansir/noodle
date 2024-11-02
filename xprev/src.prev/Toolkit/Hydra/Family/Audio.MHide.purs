module Toolkit.Hydra.Family.Audio.FHide where


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


id = Node.Family :: _ "hide"


name :: String
name = "hide"


type State = Unit


defaultState :: State
defaultState = unit


_in_audio = Fn.Input  0 :: _ "audio"

_out_out  = Fn.Output 0 :: _ "out"


type Inputs = ( todo :: H.TODO )
type Outputs = ( out :: H.TODO )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.TODO }


type Family (m :: Type -> Type) = -- {-> audio <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> audio <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            -- audio <- P.receive _in_audio
            -- --
            -- Hide a ?input
            -- P.send _out_out ?out_out
            pure unit


type Node (m :: Type -> Type) =
    N.Node "hide" State
        Inputs
        Outputs
        WrapRepr
        m