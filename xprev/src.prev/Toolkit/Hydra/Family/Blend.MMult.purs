module Toolkit.Hydra.Family.Blend.FMult where


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


id = Node.Family :: _ "mult"


name :: String
name = "mult"


type State = Unit


defaultState :: State
defaultState = unit


_in_what   = Fn.Input  0 :: _ "what"
_in_with   = Fn.Input  1 :: _ "with"
_in_amount = Fn.Input  2 :: _ "amount"

_out_out   = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, amount :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_with _in_amount


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, amount : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> blend <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> blend <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            amount <- P.receive _in_amount
            -- Mult what with amount
            P.send _out_out $ H.BlendOf { what, with } $ H.Mult amount


type Node (m :: Type -> Type) =
    N.Node "mult" State
        Inputs
        Outputs
        WrapRepr
        m