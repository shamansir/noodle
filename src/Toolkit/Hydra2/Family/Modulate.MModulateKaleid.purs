module Toolkit.Hydra2.Family.Modulate.FModulateKaleid where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "modulateKaleid"


name :: String
name = "modulateKaleid"


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, nSides :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, nSides : H.Number 3.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            nSides <- P.receive _in_nSides
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModKaleid { nSides }


type Node (m :: Type -> Type) =
    N.Node "modulateKaleid" Unit
        Inputs
        Outputs
        m