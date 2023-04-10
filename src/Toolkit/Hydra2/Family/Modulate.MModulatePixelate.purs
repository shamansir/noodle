module Toolkit.Hydra2.Family.Modulate.FModulatePixelate where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "modulatePixelate"


name :: String
name = "modulatePixelate"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_multiple = Fn.Input :: _ "multiple"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, multiple :: H.Value, offset :: H.Value )
type Outputs = ( out :: H.Texture )


type InputsOrder :: SOrder
type InputsOrder = "what" ::: "with" ::: "multiple" ::: "offset" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, multiple : H.Number 10.0, offset : H.Number 3.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            multiple <- P.receive _in_multiple
            offset <- P.receive _in_offset
            -- ModulatePixelate what with multiple offset
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModPixelate { multiple, offset }


type Node (m :: Type -> Type) =
    N.Node "modulatePixelate" State
        Inputs
        Outputs
        m