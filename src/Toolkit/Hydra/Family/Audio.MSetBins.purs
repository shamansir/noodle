module Toolkit.Hydra.Family.Audio.FSetBins where


import Toolkit.Hydra.Types as H


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


id = Node.Family :: _ "setBins"


name :: String
name = "setBins"


type State = Unit


defaultState :: State
defaultState = unit


_in_numBins = Fn.Input 0 :: _ "numBins"


type Inputs = ( numBins :: H.Value )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_numBins


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { numBins : H.Number 4.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> audio <-}
    Family.Def State
        Inputs
        Outputs
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
            numBins <- P.receive _in_numBins
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setBins" State
        Inputs
        Outputs
        m