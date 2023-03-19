module Toolkit.Hydra2.Family.Color.FA where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "a"


name :: String
name = "a"


_in_what = Fn.Input :: _ "what"
_in_scale = Fn.Input :: _ "scale"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, scale :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, scale : H.Number 1.0, offset : H.Number 1.0 }


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
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            P.send _out_out $ H.WithColor what $ H.A { scale, offset }


type Node (m :: Type -> Type) =
    N.Node "a" Unit
        Inputs
        Outputs
        m