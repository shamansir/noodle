module Toolkit.Hydra2.Family.Audio.FFft where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "fft"


name :: String
name = "fft"


_in_audio = Fn.Input :: _ "audio"
_in_h = Fn.Input :: _ "h"

_out_out = Fn.Output :: _ "out"


type Inputs = ( audio :: H.Audio, h :: H.AudioBin )
type Outputs = ( out :: H.Value )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, h : H.H0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.None }


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
            h <- P.receive _in_h
            -- Fft a h
            P.send _out_out $ H.Audio audio h


type Node (m :: Type -> Type) =
    N.Node "fft" Unit
        Inputs
        Outputs
        m