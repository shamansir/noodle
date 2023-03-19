module Toolkit.Hydra2.Family.Audio.FSetCutoff where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "setCutoff"


name :: String
name = "setCutoff"


_in_audio = Fn.Input :: _ "audio"
_in_cutoff = Fn.Input :: _ "cutoff"


type Inputs = ( audio :: H.Audio, cutoff :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, cutoff : H.Number 2.0 }


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
            cutoff <- P.receive _in_cutoff
            -- SetCutoff a cutoff
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setCutoff" Unit
        Inputs
        Outputs
        m