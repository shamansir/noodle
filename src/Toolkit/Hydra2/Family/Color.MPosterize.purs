module Toolkit.Hydra2.Family.Color.FPosterize where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "posterize"


name :: String
name = "posterize"


_in_what = Fn.Input :: _ "what"
_in_bins = Fn.Input :: _ "bins"
_in_gamma = Fn.Input :: _ "gamma"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, bins :: H.Value, gamma :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, bins : H.Number 3.0, gamma : H.Number 0.6 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            bins <- P.receive _in_bins
            gamma <- P.receive _in_gamma
            P.send _out_out $ H.WithColor what $ H.Posterize { bins, gamma }


type Node (m :: Type -> Type) =
    N.Node "posterize" Unit
        Inputs
        Outputs
        m