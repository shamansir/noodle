module Toolkit.Hydra2.Family.Audio.FShow where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_audio = Fn.Input :: _ "audio"

_out_out = Fn.Output :: _ "out"


type Inputs = ( audio :: H.Audio, todo :: H.TODO )
type Outputs = ( out :: H.TODO )


defaultInputs :: Record Inputs
defaultInputs = { audio : H.Silence, todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.TODO }


type Family m = -- {-> audio <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "show" $ do
            audio <- P.receive _in_audio
            --
            -- Hide a ?input
            -- P.send _out_out ?out_out
            pure unit


type Node m =
    N.Node "show" Unit
        Inputs
        Outputs
        m