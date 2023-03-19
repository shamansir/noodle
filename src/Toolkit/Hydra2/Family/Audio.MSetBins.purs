module Toolkit.Hydra2.Family.Audio.FSetBins where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "setBins"


name :: String
name = "setBins"


_in_audio = Fn.Input :: _ "audio"
_in_numBins = Fn.Input :: _ "numBins"


type Inputs = ( audio :: H.Audio, numBins :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, numBins : H.Number 4.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> audio <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> audio <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            audio <- P.receive _in_audio
            numBins <- P.receive _in_numBins
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setBins" Unit
        Inputs
        Outputs
        m