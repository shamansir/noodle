module Toolkit.Hydra.Family.CAI.FSmartGradient where


import Prelude (Unit, unit, ($), bind, pure)

import Data.SOrder (SOrder, type (:::), T, s1, s4)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)
import Toolkit.Hydra.Lang.Fn as HFn


id = Node.Family :: _ "caiSmartGradient"


name :: String
name = "caiSmartGradient"


type State = Unit


defaultState :: State
defaultState = unit


_in_primary   = Fn.Input  0 :: _ "primary"
_in_secondary = Fn.Input  1 :: _ "secondary"
_in_ternary   = Fn.Input  2 :: _ "ternary"
_in_mode      = Fn.Input  4 :: _ "mode"

_out_gradient   = Fn.Output 0 :: _ "gradient"


type Inputs = ( primary :: H.Texture, secondary :: H.Texture, ternary :: H.Texture, mode :: H.Value )
type Outputs = ( gradient :: H.Texture )


inputsOrder :: _
inputsOrder = s4 _in_primary _in_secondary _in_ternary _in_mode


outputsOrder :: _
outputsOrder = s1 _out_gradient


defaultPrimary = H.Start $ H.Solid { r : H.Number 1.0, g : H.Number 0.0, b : H.Number 0.0, a : H.Number 1.0 } :: H.Texture
defaultSecondary = H.Start $ H.Solid { r : H.Number 0.0, g : H.Number 1.0, b : H.Number 0.0, a : H.Number 1.0 } :: H.Texture
defaultTernary = H.Start $ H.Solid { r : H.Number 0.0, g : H.Number 0.0, b : H.Number 1.0, a : H.Number 1.0 } :: H.Texture


defaultInputs :: Record Inputs
defaultInputs = { primary : defaultPrimary, secondary : defaultSecondary, ternary : defaultTernary, mode : H.Number 3.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { gradient : H.Empty }


type Family (m :: Type -> Type) = -- {-> caiSmartGradient <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiSmartGradient <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            primary <- P.receive _in_primary
            secondary <- P.receive _in_secondary
            ternary <- P.receive _in_primary
            mode <- P.receive _in_mode
            P.send _out_gradient
                $ H.CallGlslFn { over : H.Empty, mbWith : Nothing }
                $ H.GlslFnRef
                $ HFn.fn4 "smartGradientCAI"
                    ( "primary" /\ H.T primary )
                    ( "secondary" /\ H.T secondary )
                    ( "ternary" /\ H.T ternary )
                    ( "mode" /\ H.V mode )


type Node (m :: Type -> Type) =
    N.Node "caiSmartGradient" State
        Inputs
        Outputs
        WrapRepr
        m
