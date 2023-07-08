module Toolkit.Hydra2.Family.Synth.FSetResolution where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "setResolution"


name :: String
name = "setResolution"


type State = Unit


defaultState :: State
defaultState = unit


_in_width  = Fn.Input 0 :: _ "width"
_in_height = Fn.Input 1 :: _ "height"


type Inputs = ( width :: H.Value, height :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_width _in_height


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { width : H.None, height : H.None }


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
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            width <- P.receive _in_width
            height <- P.receive _in_height
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setResolution" State
        Inputs
        Outputs
        m