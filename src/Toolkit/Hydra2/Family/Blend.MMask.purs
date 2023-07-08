module Toolkit.Hydra2.Family.Blend.FMask where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s2)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "mask"


name :: String
name = "mask"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input  0 :: _ "what"
_in_with = Fn.Input  1 :: _ "with"

_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s2 _in_what _in_with


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> blend <-}
    Family.Def State
        Inputs
        Outputs
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
            -- Mask what with
            P.send _out_out $ H.BlendOf { what, with } $ H.Mask


type Node (m :: Type -> Type) =
    N.Node "mask" State
        Inputs
        Outputs
        m