module Toolkit.Hydra2.Family.Synth.FSetFunction where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "setFunction"


name :: String
name = "setFunction"


_in_fn = Fn.Input :: _ "fn"


type Inputs = ( fn :: H.GlslFn )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { fn : H.defaultGlslFn }


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
            fn <- P.receive _in_fn
            -- TODO
            pure unit


type Node (m :: Type -> Type) =
    N.Node "setFunction" Unit
        Inputs
        Outputs
        m