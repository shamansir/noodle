module Toolkit.Hydra2.Family.Synth.FSetResolution where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "setResolution"


name :: String
name = "setResolution"


_in_width = Fn.Input :: _ "width"
_in_height = Fn.Input :: _ "height"


type Inputs = ( width :: H.Value, height :: H.Value )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { width : H.None, height : H.None }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> synth <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> synth <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            width <- P.receive _in_width
            height <- P.receive _in_height
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setResolution" Unit
        Inputs
        Outputs
        m