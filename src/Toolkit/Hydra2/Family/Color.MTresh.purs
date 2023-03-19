module Toolkit.Hydra2.Family.Color.FTresh where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "tresh"


name :: String
name = "tresh"


_in_what = Fn.Input :: _ "what"
_in_treshold = Fn.Input :: _ "treshold"
_in_tolerance = Fn.Input :: _ "tolerance"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, treshold :: H.Value, tolerance :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, treshold : H.Number 0.5, tolerance : H.Number 0.1 }


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
            treshold <- P.receive _in_treshold
            tolerance <- P.receive _in_tolerance
            P.send _out_out $ H.WithColor what $ H.Tresh { treshold, tolerance }


type Node (m :: Type -> Type) =
    N.Node "tresh" Unit
        Inputs
        Outputs
        m