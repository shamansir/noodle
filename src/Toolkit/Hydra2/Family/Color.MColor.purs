module Toolkit.Hydra2.Family.Color.FColor where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "color"


name :: String
name = "color"


_in_what = Fn.Input :: _ "what"
_in_r = Fn.Input :: _ "r"
_in_g = Fn.Input :: _ "g"
_in_b = Fn.Input :: _ "b"
_in_a = Fn.Input :: _ "a"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, r :: H.Value, g :: H.Value, b :: H.Value, a :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, r : H.Number 1.0, g : H.Number 1.0, b : H.Number 1.0, a : H.Number 1.0 }


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
            r <- P.receive _in_r
            g <- P.receive _in_g
            b <- P.receive _in_b
            a <- P.receive _in_a
            P.send _out_out $ H.WithColor what $ H.Color { r, g, b, a }


type Node (m :: Type -> Type) =
    N.Node "color" Unit
        Inputs
        Outputs
        m