module Toolkit.Hydra.Family.Modulate.FModulateKaleid where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "modulateKaleid"


name :: String
name = "modulateKaleid"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_with   = Fn.Input  1 :: _ "with"
_in_nSides = Fn.Input  2 :: _ "nSides"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, nSides :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_with _in_nSides


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, nSides : H.Number 3.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            nSides <- P.receive _in_nSides
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModKaleid { nSides }


type Node (m :: Type -> Type) =
    N.Node "modulateKaleid" State
        Inputs
        Outputs
        WrapRepr
        m